;; s3-publish.el --- Publish files to S3 from Emacs -*- lexical-binding: t; -*-
;;
;; Author: EnigmaCurry
;; URL: https://github.com/EnigmaCurry/s3-publish.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: s3, publish
;; SPDX-License-Identifier: 0BSD
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This enables publishing files (e.g., HTML) to S3 (compatible)
;; storage.

;;; Usage:
;;

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'ox-html)
(require 'url-parse)

(defgroup s3-publish nil
  "Settings for publishing files to S3-compatible storage."
  :prefix "s3-publish-"
  :group 'external)


(defun s3-publish-get-credentials (profile)
  "Retrieve S3 credentials for the given PROFILE.
The PROFILE is expected to be a plist containing at least a :name key.
The auth-source machine name will be derived as \"s3-publish-<profile-name>\".
Returns a new plist combining the original PROFILE
with keys :access-key and :secret-key.
Signals an error if no credentials are found."
  (let* ((profile-name (plist-get profile :name))
         (machine (concat "s3-publish-" profile-name))
         (results (auth-source-search :host machine
                                      :require '(:user :secret)
                                      :max 1))
         (entry (car results)))
    (if entry
        (let* ((access-key (plist-get entry :user))
               (secret (let ((sec (plist-get entry :secret)))
                         (if (functionp sec)
                             (funcall sec)
                           sec))))
          (append profile (list :access-key access-key :secret-key secret)))
      (error "No credentials found for s3-publish profile: %s" profile-name))))

(defvar s3-publish-profile-history nil
  "Minibuffer history of S3 profile names chosen at the profile prompt.
Its first element is the profile used last, which the prompt starts on.
`savehist-mode' saves it across sessions along with other histories.")

(defun s3-publish--profile-names ()
  "Return the names of the profiles in `s3-publish-profiles'."
  (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))

(defun s3-publish--default-profile-name (names)
  "Return the profile among NAMES that the profile prompt starts on.
That is the profile used last, if it still exists, then the one named
\"default\", then the first."
  (or (seq-find (lambda (name) (member name names)) s3-publish-profile-history)
      (car (member "default" names))
      (car names)))

(defun s3-publish--read-profile-name ()
  "Prompt for the name of a profile in `s3-publish-profiles'.
The prompt starts on the profile used last, so pressing RET reuses it;
it is never chosen without asking."
  (let* ((names (s3-publish--profile-names))
         (default (s3-publish--default-profile-name names)))
    (let ((name (completing-read (if (fboundp 'format-prompt)
                                     (format-prompt "Select S3 profile" default)
                                   "Select S3 profile: ")
                                 names nil t nil
                                 's3-publish-profile-history default)))
      ;; Accepting the default with an empty RET leaves the history as it
      ;; was, so record the choice here to make it the next default.
      (add-to-history 's3-publish-profile-history name)
      name)))

(defun s3-publish-get-profile (name)
  "Return the S3 publishing profile with NAME, minus credentials.
If no profile with the given NAME exists in `s3-publish-profiles',
an error is signaled."
  (let ((profile (seq-find (lambda (p)
                             (string= (plist-get p :name) name))
                           s3-publish-profiles)))
    (unless profile
      (error "No s3-publish profile found with name: %s" name))
    profile))

(defvar s3-publish-profiles-buffer "*s3-publish-profiles*"
  "Buffer name for displaying s3-publish profiles.")


(defun s3-publish-manage-profiles ()
  "Manage s3-publish profiles interactively.
Immediately displays a list of profiles in a dedicated buffer.
Then, prompts you to Add, Edit, Remove, or Done.
Selecting Done exits the management session, closing the profiles window and
killing the buffer."
  (interactive)
  (s3-publish-refresh-profiles-buffer)
  (catch 'done
    (while t
      (let ((action (completing-read
                     "Manage s3-publish profiles (Add/Edit/Remove/Done): "
                     '("Add" "Edit" "Remove" "Done") nil t)))
        (cond
         ((string= action "Add")
          (s3-publish-add-profile))
         ((string= action "Edit")
          (s3-publish-edit-profile))
         ((string= action "Remove")
          (s3-publish-remove-profile))
         ((string= action "Done")
          (throw 'done nil)))
        (s3-publish-refresh-profiles-buffer))))
  (let ((win (get-buffer-window s3-publish-profiles-buffer)))
    (when win
      (delete-window win)))
  (kill-buffer s3-publish-profiles-buffer)
  (message "s3-publish profiles saved."))

(defun s3-publish-list-profiles ()
  "List all s3-publish profiles in a temporary buffer."
  (if s3-publish-profiles
      (with-output-to-temp-buffer "*s3-publish-profiles*"
        (dolist (p s3-publish-profiles)
          (princ (format
              "Name: %s\nEndpoint: %s\nBucket: %s\nPublic ACL: %s\nSalt: %s\n\n"
                  (plist-get p :name)
                  (plist-get p :endpoint)
                  (plist-get p :bucket)
                  (if (plist-get p :public-acl) "Yes" "No")
                  (or (plist-get p :salt) "Not set")))))
    (message "No s3-publish profiles defined.")))


(defun s3-publish-remove-profile ()
  "Remove an existing s3-publish profile interactively."
  (let* ((names (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (name (completing-read "Remove profile: " names nil t)))
    (setq s3-publish-profiles
          (cl-remove-if (lambda (p)
                          (string= (plist-get p :name) name))
                        s3-publish-profiles))
    (s3-publish-save-profiles)
    (message "Profile '%s' removed." name)
    (s3-publish-refresh-profiles-buffer)))


(defun s3-publish-save-profiles ()
  "Persist the current value of `s3-publish-profiles` to the custom file."
  (customize-save-variable 's3-publish-profiles s3-publish-profiles))

(defun s3-publish-get-file-key (file salt)
  "Generate an S3 key for FILE using SALT.
FILE's absolute path is first expanded, then concatenated with SALT.
The resulting string is hashed using MD5, then converted to a base-36 string.
Dashes are inserted every 5 characters. If the file has an extension,
it is appended to the key."
  (let* ((abs-path (expand-file-name file))
         (input (concat salt abs-path))
         (md5-hex (md5 input))
         (hash-num (string-to-number md5-hex 16))
         (chars "0123456789abcdefghijklmnopqrstuvwxyz"))
    (cl-labels ((int-to-base36 (n)
                  "Convert integer N to a base-36 string."
                  (if (< n 36)
                      (substring chars n (1+ n))
                    (concat (int-to-base36 (floor n 36))
                            (substring chars (mod n 36) (1+ (mod n 36))))))
                (insert-dashes (s n)
                  "Insert a dash every N characters into string S."
                  (let ((result "")
                        (i 0)
                        (len (length s)))
                    (while (< i len)
                 (setq result (concat result (substring s i (min len (+ i n)))))
                      (setq i (+ i n))
                      (unless (>= i len)
                        (setq result (concat result "-"))))
                    result)))
      (let* ((base36 (int-to-base36 hash-num))
             (key (insert-dashes base36 5))
             (ext (file-name-extension abs-path t))) ; t means include the dot
        (concat key ext)))))

(defun s3-publish-generate-s3cmd-config (profile)
  "Generate a temporary s3cmd config file from PROFILE.
PROFILE should be a plist containing at least:
  :access-key, :secret-key, :endpoint, and :bucket.
The generated config file will have a [default] section with:
  access_key = <access-key>
  host_base = <host-base>
  host_bucket = %(bucket)s.<host-base>
  secret_key = <secret-key>
  website_endpoint = http://%(bucket)s.<host-base>/
If access-key or secret-key is nil, an error is signaled.
The function removes the protocol, trailing slash, and a bucket subdomain
if the endpoint starts with '<bucket>.'.
Returns the path to the temporary config file."
  (let* ((access-key (plist-get profile :access-key))
         (secret-key (plist-get profile :secret-key))
         (endpoint (plist-get profile :endpoint))
         (bucket (plist-get profile :bucket)))
    (unless (and access-key secret-key)
     (error "Both :access-key and :secret-key must be provided in the profile"))
    ;; Remove protocol (http:// or https://)
    (let* ((host-base (replace-regexp-in-string "^https?://" "" endpoint))
           ;; Remove any trailing slash.
           (host-base (replace-regexp-in-string "/$" "" host-base))
           ;; If host-base starts with "<bucket>.", remove that subdomain.
           (host-base (if (string-prefix-p (concat bucket ".") host-base)
               (replace-regexp-in-string
                  (concat "^" (regexp-quote (concat bucket "."))) "" host-base)
                        host-base))
           (config-content (format "[default]
access_key = %s
host_base = %s
host_bucket = %%(bucket)s.%s
secret_key = %s
website_endpoint = http://%%(bucket)s.%s/
" access-key host-base host-base secret-key host-base))
           (tmp-file (make-temp-file "s3cmd-config-" nil ".s3cfg")))
      (with-temp-file tmp-file
        (insert config-content))
      tmp-file)))

;;; Running s3cmd -------------------------------------------------------------
;; Every s3cmd call goes through `s3-publish--s3cmd', which writes the
;; credentials config, runs the command, and deletes the config again, so
;; secret keys never pile up in the temporary directory.

(defcustom s3-publish-cache-control "no-cache"
  "Cache-Control header sent with every object s3-publish uploads.

The default, \"no-cache\", lets browsers and CDNs keep a copy but makes
them check with the bucket before using it.  That costs one conditional
request, and means a republished page, the HTML index and the RSS feed
show their new contents straight away rather than whenever a cache
expires.  Use e.g. \"max-age=300\" to trade freshness for fewer requests.

nil sends no header, leaving caching to the provider's defaults."
  :type '(choice (string :tag "Header value")
                 (const :tag "No header" nil))
  :group 's3-publish)

(defun s3-publish--s3cmd (profile &rest args)
  "Run s3cmd with ARGS using PROFILE's credentials.
Returns (EXIT-CODE . OUTPUT), OUTPUT being stdout and stderr together.
The config file holding the credentials is deleted before returning."
  (let ((config (s3-publish-generate-s3cmd-config profile)))
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code (apply #'call-process "s3cmd" nil t nil
                                  "--config" config args)))
            (cons exit-code (buffer-string))))
      (when (file-exists-p config)
        (delete-file config)))))

(defun s3-publish--s3cmd-or-error (profile what &rest args)
  "Run s3cmd with ARGS using PROFILE's credentials, returning its output.
Signals an error mentioning WHAT if s3cmd fails."
  (let ((result (apply #'s3-publish--s3cmd profile args)))
    (unless (zerop (car result))
      (error "%s failed: %s" what (string-trim (cdr result))))
    (cdr result)))

(defun s3-publish--put (profile file key &optional mime-type temporary)
  "Upload FILE to KEY in PROFILE's bucket, signalling an error on failure.
MIME-TYPE, when non-nil, overrides s3cmd's guess.  TEMPORARY tags the
object `temp=temp', the tag s3-publish gives published content (as
opposed to the index and feed).  Applies the profile's ACL and
`s3-publish-cache-control'."
  (apply #'s3-publish--s3cmd-or-error profile (format "Upload of %s" key)
         (append (and (plist-get profile :public-acl) (list "--acl-public"))
                 (and mime-type (list (concat "--mime-type=" mime-type)))
                 (and s3-publish-cache-control
                      (list (concat "--add-header=Cache-Control: "
                                    s3-publish-cache-control)))
                 (and temporary (list "--add-header=x-amz-tagging: temp=temp"))
                 (list "put" file
                       (format "s3://%s/%s" (plist-get profile :bucket) key)))))

(defun s3-publish--put-string (profile string key mime-type)
  "Upload STRING as the object KEY in PROFILE's bucket, with MIME-TYPE."
  (let ((file (make-temp-file "s3-publish-" nil
                              (file-name-extension key t))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8))
            (write-region string nil file nil 'quiet))
          (s3-publish--put profile file key mime-type))
      (delete-file file))))

(defun s3-publish--get (profile key)
  "Return the contents of KEY in PROFILE's bucket, or nil if it is missing."
  (let ((file (make-temp-file "s3-publish-get-")))
    (unwind-protect
        (let ((result (s3-publish--s3cmd
                       profile "get" "--force"
                       (format "s3://%s/%s" (plist-get profile :bucket) key)
                       file)))
          (when (and (zerop (car result))
                     (not (string-match-p "WARNING: Not Found" (cdr result)))
                     (> (file-attribute-size (file-attributes file)) 0))
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8))
                (insert-file-contents file))
              (buffer-string))))
      (when (file-exists-p file)
        (delete-file file)))))


;; Add RSS feed support to profile settings
(defcustom s3-publish-profiles nil
  "List of S3 publishing profiles.
Each profile is an alist containing the following keys:
  - :name       A unique identifier for the profile.
  - :endpoint   The domain of the S3-compatible endpoint (minus bucket name).
  - :bucket     The bucket name to which files will be published.
  - :public-acl A boolean indicating if the file is public.
  - :salt       A random string used to salt file keys (generated when added).
  - :rss-feed   A boolean indicating if an RSS feed should be maintained.
  - :rss-title  The title of the RSS feed.
  - :rss-desc   The description of the RSS feed.
  - :rss-link   The base URL for the RSS feed (usually the bucket URL).
The auth-source machine name will be derived by prepending \"s3-publish-\" to
the profile name.
Example:
  ((:name \"default\"
     :endpoint \"s3.amazonaws.com\"
     :bucket \"my-bucket\"
     :public-acl t
     :salt \"1a2b3c4d\"
     :rss-feed t
     :rss-title \"My Public Files\"
     :rss-desc \"Files published from Emacs\"
     :rss-link \"https://my-bucket.s3.amazonaws.com\")
    (:name \"do-spaces\"
     :endpoint \"nyc3.digitaloceanspaces.com\"
     :bucket \"do-bucket\"
     :public-acl nil
     :salt \"9e8f7d6c\"
     :rss-feed nil))
"
  :group 's3-publish
  :type '(repeat
          (alist :key-type symbol
                 :value-type (choice
                              (string :tag "String")
                              (boolean :tag "Boolean")))))

;; Extend profile display function to show RSS settings
(defun s3-publish-refresh-profiles-buffer ()
  "Refresh the s3-publish profiles list in the dedicated buffer.
If the buffer does not exist, it is created."
  (with-current-buffer (get-buffer-create s3-publish-profiles-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if s3-publish-profiles
          (dolist (p s3-publish-profiles)
            (insert (format
                     "Name: %s\nEndpoint: %s\nBucket: %s\nPublic ACL: %s\nSalt: %s\n"
                     (plist-get p :name)
                     (plist-get p :endpoint)
                     (plist-get p :bucket)
                     (if (plist-get p :public-acl) "Yes" "No")
                     (or (plist-get p :salt) "Not set")))
            (let ((rss-feed (plist-get p :rss-feed)))
              (insert (format "RSS Feed: %s\n" (if rss-feed "Yes" "No")))
              (when rss-feed
                (insert (format "  - Title: %s\n  - Description: %s\n  - Link: %s\n"
                                (or (plist-get p :rss-title) "Not set")
                                (or (plist-get p :rss-desc) "Not set")
                                (or (plist-get p :rss-link) "Not set")))))
            (insert "\n"))
        (insert "No s3-publish profiles defined."))
      (goto-char (point-min))
      (read-only-mode 1)))
  (display-buffer s3-publish-profiles-buffer))

;; Extend profile add function to include RSS settings
(defun s3-publish-add-profile ()
  "Add a new s3-publish profile interactively.
Generates a random salt (an 8-character string) and stores it with the profile."
  (let* ((name (read-string "Profile name: "))
         (endpoint (read-string "Endpoint URL: "))
         (bucket (read-string "Bucket name: "))
         (public-acl (y-or-n-p "Make files public? "))
         (salt (substring (md5 (format "%s-%d" (current-time-string) (random)))
                          0 32))
         (rss-feed (y-or-n-p "Maintain an RSS feed for published files? "))
         (new-profile (list :name name
                            :endpoint endpoint
                            :bucket bucket
                            :public-acl public-acl
                            :salt salt
                            :rss-feed rss-feed)))

    ;; If RSS feed is enabled, prompt for additional settings
    (when rss-feed
      (let* ((rss-title (read-string "RSS feed title: " (format "%s Files" name)))
            (rss-desc (read-string "RSS feed description: " "Files published from Emacs"))
            (host-base (replace-regexp-in-string "^https?://" "" endpoint))
            (host-base (replace-regexp-in-string "/$" "" host-base))
            (host-base (if (string-prefix-p (concat bucket ".") host-base)
                           (replace-regexp-in-string
                            (concat "^" (regexp-quote (concat bucket "."))) "" host-base)
                         host-base)))
        (let ((default-link (format "https://%s.%s" bucket host-base)))
          (setq new-profile
                (append new-profile
                        (list :rss-title rss-title
                              :rss-desc rss-desc
                              :rss-link (read-string "RSS feed base URL: " default-link)))))))

    (setq s3-publish-profiles (append s3-publish-profiles (list new-profile)))
    (s3-publish-save-profiles)
    (message "Profile '%s' added." name)
    (s3-publish-refresh-profiles-buffer)))

;; Extend profile edit function to include RSS settings
(defun s3-publish-edit-profile ()
  "Edit an existing s3-publish profile interactively."
  (let* ((names (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (name (completing-read "Edit profile: " names nil t))
         (profile (seq-find (lambda (p)
                              (string= (plist-get p :name) name))
                            s3-publish-profiles)))
    (if profile
        (let* ((new-endpoint
                (read-string (format "Endpoint URL (current: %s): "
                                     (plist-get profile :endpoint))
                             nil nil (plist-get profile :endpoint)))
               (new-bucket
                (read-string (format "Bucket name (current: %s): "
                                     (plist-get profile :bucket))
                             nil nil (plist-get profile :bucket)))
               (new-public-acl
                (y-or-n-p (format "Make files public? (current: %s): "
                                  (if (plist-get profile :public-acl) "Yes" "No"))))
               (rss-enabled (plist-get profile :rss-feed))
               (new-rss-feed
                (y-or-n-p (format "Maintain RSS feed? (current: %s): "
                                  (if rss-enabled "Yes" "No"))))
               ;; Retain the existing salt
               (edited-profile (list :name name
                                     :endpoint new-endpoint
                                     :bucket new-bucket
                                     :public-acl new-public-acl
                                     :salt (plist-get profile :salt)
                                     :rss-feed new-rss-feed)))

          ;; If RSS feed is newly enabled or was already enabled
          (when new-rss-feed
            (let* ((current-title (or (plist-get profile :rss-title) ""))
                   (current-desc (or (plist-get profile :rss-desc) ""))
                   (current-link (or (plist-get profile :rss-link) ""))
                   (new-title
                    (read-string (format "RSS feed title (current: %s): "
                                         (if (string= current-title "") "Not set" current-title))
                                 nil nil (if (string= current-title "")
                                             (format "%s Files" name)
                                           current-title)))
                   (new-desc
                    (read-string (format "RSS feed description (current: %s): "
                                         (if (string= current-desc "") "Not set" current-desc))
                                 nil nil (if (string= current-desc "")
                                             "Files published from Emacs"
                                           current-desc)))
                   (host-base (replace-regexp-in-string "^https?://" "" new-endpoint))
                   (host-base (replace-regexp-in-string "/$" "" host-base))
                   (host-base (if (string-prefix-p (concat new-bucket ".") host-base)
                                  (replace-regexp-in-string
                                   (concat "^" (regexp-quote (concat new-bucket "."))) "" host-base)
                                host-base))
                   (default-link (if (string= current-link "")
                                     (format "https://%s.%s" new-bucket host-base)
                                   current-link))
                   (new-link
                    (read-string (format "RSS feed base URL (current: %s): "
                                         (if (string= current-link "") "Not set" current-link))
                                 nil nil default-link)))
              (setq edited-profile
                    (append edited-profile
                            (list :rss-title new-title
                                  :rss-desc new-desc
                                  :rss-link new-link)))))

          (setq s3-publish-profiles
                (mapcar (lambda (p)
                          (if (string= (plist-get p :name) name)
                              edited-profile
                            p))
                        s3-publish-profiles))
          (s3-publish-save-profiles)
          (message "Profile '%s' updated." name))
      (message "Profile '%s' not found." name))
    (s3-publish-refresh-profiles-buffer)))

;; RSS feed management functions
(defconst s3-publish-rss-filename "index.xml"
  "Filename for the RSS feed XML file.")

(defconst s3-publish-html-index-filename "index.html"
  "Filename for the HTML index file.")

(defun s3-publish-format-rfc822-date (time)
  "Format TIME as an RFC822 date string for RSS."
  (format-time-string "%a, %d %b %Y %T %z" time))


(defun s3-publish-download-feed (profile)
  "Download the current RSS feed for PROFILE from S3.
Returns the feed content as a string if successful,
or nil if the feed doesn't exist yet."
  (let ((content (s3-publish--get profile s3-publish-rss-filename)))
    (if content
        (message "Downloaded existing RSS feed")
      (message "No existing RSS feed found, will create new one"))
    content))

;; Similarly fix the HTML index download function
(defun s3-publish-download-html-index (profile)
  "Download the current HTML index for PROFILE from S3.
Returns the index content as a string if successful,
or nil if the index doesn't exist yet."
  (let ((content (s3-publish--get profile s3-publish-html-index-filename)))
    (if content
        (message "Downloaded existing HTML index")
      (message "No existing HTML index found, will create new one"))
    content))

(defun s3-publish-create-new-rss-feed (profile)
  "Create a new RSS feed XML document for PROFILE."
  (let ((title (or (plist-get profile :rss-title) 
                   (format "%s Files" (plist-get profile :name))))
        (description (or (plist-get profile :rss-desc) 
                         "Files published from Emacs"))
        (link (or (plist-get profile :rss-link)
                  (let* ((bucket (plist-get profile :bucket))
                         (endpoint (plist-get profile :endpoint))
                         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
                         (host-base (replace-regexp-in-string "/$" "" host-base)))
                    (format "https://%s.%s" bucket host-base)))))
    (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>%s</title>
    <description>%s</description>
    <link>%s</link>
    <atom:link href=\"%s/%s\" rel=\"self\" type=\"application/rss+xml\" />
    <pubDate>%s</pubDate>
    <lastBuildDate>%s</lastBuildDate>
    <generator>s3-publish.el</generator>
  </channel>
</rss>"
            title
            description
            link
            link
            s3-publish-rss-filename
            (s3-publish-format-rfc822-date (current-time))
            (s3-publish-format-rfc822-date (current-time)))))

(defun s3-publish-create-new-html-index (profile)
  "Create a new HTML index page for PROFILE."
  (let ((title (or (plist-get profile :rss-title)
                   (format "%s Files" (plist-get profile :name))))
        (description (or (plist-get profile :rss-desc)
                         "Files published from Emacs"))
        (link (or (plist-get profile :rss-link)
                  (let* ((bucket (plist-get profile :bucket))
                         (endpoint (plist-get profile :endpoint))
                         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
                         (host-base (replace-regexp-in-string "/$" "" host-base)))
                    (format "https://%s.%s" bucket host-base)))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>%s</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; line-height: 1.5; }
    h1 { margin-bottom: 10px; }
    .description { color: #555; margin-bottom: 30px; font-style: italic; }
    .entry { margin-bottom: 25px; border-bottom: 1px solid #eee; padding-bottom: 20px; }
    .entry h2 { margin-bottom: 5px; }
    .entry .date { color: #666; font-size: 0.9em; margin-bottom: 10px; }
    .entry .link { word-break: break-all; }
    .footer { margin-top: 30px; font-size: 0.8em; color: #666; }
    .subscribe { display: inline-block; background: #f5f5f5; padding: 2px 8px; border-radius: 3px; text-decoration: none; color: #444; margin-top: 5px; }
    .subscribe:hover { background: #e5e5e5; }
  </style>
</head>
<body>
  <h1>%s</h1>
  <div class=\"description\">%s</div>
  <div class=\"entries\">
    <!-- Entries will be added here -->
  </div>
  <div class=\"footer\">
    <p>Generated by <a href=\"https://github.com/EnigmaCurry/s3-publish.el\">s3-publish.el</a></p>
    <p><a href=\"%s\" class=\"subscribe\">RSS Feed</a></p>
  </div>
</body>
</html>"
            title
            title
            description
            (concat link "/" s3-publish-rss-filename))))

(defun s3-publish-upload-file (file profile &optional title description)
  "Upload FILE to S3 using PROFILE.
PROFILE is a plist that must include :bucket, :endpoint, and credentials
(:access-key and :secret-key), and :salt.
The file's key is generated using `s3-publish-get-file-key' with the
profile's salt.
Optional TITLE and DESCRIPTION are used for RSS feed entries if the profile has
RSS feed enabled. If not provided, they will be generated from the filename.
If the upload is successful and the profile's :public-acl is non-nil,
this function returns the public URL for the file (constructed as
  https://BUCKET.HOST_BASE/KEY).
If :public-acl is nil, it returns the S3 URI (e.g., s3://BUCKET/KEY).
If the upload fails, it signals an error with the error message."
  (let* ((key (s3-publish-get-file-key file (plist-get profile :salt)))
         (url (s3-publish--object-url profile key)))
    (s3-publish--put profile file key nil t)
    (when (and (plist-get profile :rss-feed)
               (plist-get profile :public-acl))
      (s3-publish-update-feed
       profile url
       (or title (file-name-nondirectory file))
       (or description
           (format "File published on %s"
                   (format-time-string "%Y-%m-%d %H:%M")))))
    url))

;;; HTML theme ----------------------------------------------------------------
;; Org's export stylesheet is deliberately minimal, so published documents
;; look unstyled unless they carry a theme of their own.  Carrying it in the
;; document does not work here anyway: a `#+SETUPFILE:' naming a stylesheet
;; next to the source cannot resolve for Markdown, which is exported from a
;; temporary directory.  The theme therefore belongs to the publisher, and is
;; inlined into every page so published documents stay self-contained.

(defconst s3-publish-html-default-style "\
html, body {
    margin: 0;
    padding: 0;
    min-height: 100vh;
}
body {
	background: #121212;
	color: #e0e0e0;
	font-family: \"Bitstream Vera Sans\",Verdana,sans-serif;
    display: flex;
    flex-direction: column;
    max-width: 90em;
    justify-content: center;
    margin:0 auto;
}

/* 1.5em is loud where inline code is dense, which Markdown tends to be. */
code {
	color: #ffd269;
	font-weight: bold;
	font-size: 0.95em;
	margin: 0 0.25em 0 0.25em;
}

div#content {
	border: 1px solid #666;
	background: #1e1e1e;
	padding: 2em;
    max-width: none;
    margin: 0;
    margin-bottom: auto;
}

a {
	color: #62afff;
	text-decoration: none;
	padding: 1px
}

a:hover {
	color: #ff5370
}

#table-of-contents {
	margin: 1em 0;
	padding: .1em
}

div#content div#org-div-home-and-up {
	background: #001323;
	color: #fff
}

div#org-div-home-and-up a:link,div#org-div-home-and-up a:visited {
	color: #fff;
	background: #001323
}

div#org-div-home-and-up a:hover {
	color: #ff5370
}

div.title {
	margin: -1em -1em 0;
	font-size: 200%;
	font-weight: bold;
	background: #001323;
	color: #fff;
	padding: .75em 1em;
	font-family: \"BitStream Vera Sans\",Verdana;
	letter-spacing: .1em
}

h1 {
	background: #001323;
	color: #fff;
	font-size: 200%;
	font-weight: bold;
	letter-spacing: .1em;
	margin: -1em -1em .2em;
	padding: .75em 1em
}

h2 {
	font-size: 180%;
	border-bottom: 1px solid #555;
	padding: .2em
}

h3,h4 {
	font-size: 120%;
	border-bottom: 1px solid #444
}

tt {
	color: #39f
}

.verbatim {
	margin: .5em 0
}

pre.src-shell {
    background: #0f1015;
}

pre.src-example {
    background: #282928 !important;
}

pre {
	font-weight: bolder;
	border: none;
	padding: 1em;
	color: #e0e0e0;
	margin: 1em 0 1em 0;
	padding: 5pt;
	color: #e0e0e0;
	font-family: courier,monospace;
	font-size: 0.8em;
	background: #181f29;
    max-width: 90.5em;
    border-bottom: 2px solid #573b3b;
}

.verbatim-caption {
	border: 1px solid #555;
	background: #222;
	display: block;
	font-size: 80%;
	padding: .2em
}

div#postamble {
	text-align: left;
	color: #999;
	font-size: 80%;
	padding: 0;
	margin: 0
}

div#postamble a {
	color: #62afff
}

div#postamble a:hover {
	color: #ff5370
}

table {
	font-size: 100%;
	border-collapse: collapse;
	margin: .5em 0;
	color: #e0e0e0
}

th,td {
	border: 1px solid #555;
	padding: .3em;
	margin: 2px
}

th {
	background: #333
}

span.underline {
	text-decoration: underline
}

.fixme {
	background: #ff9800;
	font-weight: bold
}

.ra {
	text-align: right
}

.sidebar {
	float: right;
	width: 25em;
	background-color: #6a1b9a;
	color: #fff;
	margin: 2em -2em 2em 2em;
	padding: 1em
}

.sidebar a {
	border: none
}

.sidebar a:link {
	color: #80deea
}

.sidebar a:visited {
	color: #26c6da
}

.sidebar a:hover {
	color: #ffcc80
}

.sidebar a:active {
	color: #ff5370
}

.title {
	text-align: center
}

.todo {
	color: #f44336
}

.done {
	color: #4caf50
}

.timestamp {
	color: #9e9e9e
}

.timestamp-kwd {
	color: #f48fb1
}

.tag {
	background-color: #1e88e5;
	font-weight: normal
}

.target {
	background-color: #8e24aa
}

.org-ul li {
	padding-top: 0.5em;
}

details {
	margin: 2em;
}

/* Fixed: this rule used to nest `h1.title' inside `div#content' and leave the
   @media block unterminated, which swallowed every rule after it. */
@media screen and (max-width:768px) {
	div#content {
		padding: 0 0.5em 0.5em 0.5em;
	}
	h1.title {
		margin-top: 0;
	}
}

/* --- Additions ---------------------------------------------------------- */
/* Shared verbatim with the copy embedded in s3-publish.el as
   `s3-publish-html-default-style'.  Keep the two in step. */

/* Dark-only theme: match form controls and scrollbars to it. */
:root { color-scheme: dark; }

/* `org-html-self-link-headlines' wraps each heading's text in a link to its
   own anchor, so clicking a heading puts that anchor in the address bar.
   Headings must still read as headings, so the link only shows on hover. */
h1 a, h2 a, h3 a, h4 a, h5 a, h6 a {
	color: inherit;
	text-decoration: none;
}
h2 a:hover, h3 a:hover, h4 a:hover, h5 a:hover, h6 a:hover {
	color: #62afff;
}
h2 a:hover::after, h3 a:hover::after, h4 a:hover::after,
h5 a:hover::after, h6 a:hover::after {
	content: \" #\";
	color: #ff5370;
	font-weight: normal;
}
/* Keep a linked heading clear of a fixed-position browser chrome and give
   :target a visible landing cue. */
h1, h2, h3, h4, h5, h6 { scroll-margin-top: 1em; }
:target { scroll-margin-top: 1em; }

/* Org emits `pre.example' for a block with no language, which is what a
   Markdown fence without a language becomes.  The source theme's
   `pre.src-example' rule never matches any Org output. */
pre.example {
	background: #282928;
}

/* Org 9.7 and later wrap a block's text in `<pre><code>'.  The inline
   `code' rule must not reach it: its horizontal margin indents only the
   first line, since an inline element's margin sits before its first box. */
pre code {
	margin: 0;
	color: inherit;
	font-weight: inherit;
	font-size: inherit;
}

/* Source block language label.  s3-publish tags each `pre.src' with a
   `data-lang' attribute, since CSS cannot read the language out of Org's
   `src-LANG' class. */
pre.src[data-lang]::before {
	content: attr(data-lang);
	display: block;
	margin: -5pt -5pt 5pt -5pt;
	padding: 0.1em 1em 0.2em 0.5em;
	background: #0d0d0d;
	border-bottom: 2px solid #573b3b;
	color: #606c71;
	font-family: sans-serif;
	font-weight: bold;
	font-size: 0.9em;
}

/* The copy button `s3-publish-html-copy-button' adds to each code block.
   Its script positions it at the top right, over the language label. */
button.s3-publish-copy {
	padding: 0 0.6em;
	line-height: 1.2;
	border: 1px solid #555;
	border-radius: 4px;
	background: #181f29;
	color: #e0e0e0;
	font-family: sans-serif;
	font-size: 0.7em;
	font-weight: bold;
	opacity: 0.6;
}
.s3-publish-code:hover button.s3-publish-copy,
button.s3-publish-copy:focus-visible {
	opacity: 1;
}
button.s3-publish-copy:hover {
	border-color: #ff5370;
}

/* Images out of Markdown carry no size constraint, so cap them. */
#content img {
	max-width: 100%;
	height: auto;
}

/* The source-file link s3-publish appends to the bottom of every page. */
#org-download {
	margin-top: 2em;
	padding-top: 1em;
	border-top: 1px solid #555;
	font-size: 90%;
}
#org-download a {
	display: inline-block;
	padding: 0.4em 0.9em;
	border: 1px solid #555;
	border-radius: 5px;
	background: #181f29;
}
#org-download a:hover {
	border-color: #ff5370;
}

/* Source block syntax colours, for exports where htmlize emits classes
   rather than inline colours, i.e. `org-html-htmlize-output-type' set to
   `css' -- which is what s3-publish publishes with. */
pre .org-keyword, pre .org-preprocessor { color: #ff7b72; }
pre .org-string, pre .org-doc { color: #a5d6ff; }
pre .org-comment, pre .org-comment-delimiter { color: #8b949e; font-style: italic; }
pre .org-function-name { color: #d2a8ff; }
pre .org-variable-name { color: #ffa657; }
pre .org-type, pre .org-constant { color: #79c0ff; }
pre .org-builtin { color: #d2a8ff; }
pre .org-warning { color: #ffa198; }

/* A contents list headed by the document's own title instead of the generic
   \"Table of Contents\", which is what s3-publish rewrites it to.  Inert on a
   page that does not carry the class. */
#table-of-contents h2.toc-title {
	font-size: 110%;
	margin: 0 0 .5em 0;
	padding: 0 0 .3em 0;
	border-bottom: 1px solid #555;
	letter-spacing: 0;
}
#table-of-contents h2.toc-title a {
	color: #e0e0e0;
	text-decoration: none;
}
#table-of-contents h2.toc-title a:hover {
	color: #62afff;
}
#text-table-of-contents ul ul {
	padding-left: 1.1em;
}

/* On a wide screen the table of contents moves into its own sticky,
   scrollable column on the right, narrower than the content, so it stays
   visible however far you scroll.

   Org renders the TOC as a sibling inside #content, so #content becomes a
   two column grid: every child lands in column 1, the TOC is pinned to
   column 2, and the title banner spans both.  The TOC spans far more rows
   than any document has, which is what gives the sticky box room to travel
   -- the surplus rows are empty, and with no row gap they add no height, so
   the grid must never set `gap' or `row-gap', only `column-gap'.

   `:has()' keeps the sidebar off documents exported without a TOC, which
   would otherwise leave an empty column.  A browser without `:has()' drops
   the whole block and keeps the single column layout. */
@media screen and (min-width: 1100px) {
	div#content:has(> #table-of-contents) {
		display: grid;
		grid-template-columns: minmax(0, 1fr) 17em;
		column-gap: 2.5em;
	}
	div#content:has(> #table-of-contents) > * {
		grid-column: 1;
		min-width: 0;
	}
	div#content:has(> #table-of-contents) > h1.title,
	div#content:has(> #table-of-contents) > div.title {
		grid-column: 1 / -1;
	}
	div#content > #table-of-contents {
		grid-column: 2;
		grid-row: 2 / span 500;
		align-self: start;
		position: sticky;
		top: 1.5em;
		max-height: calc(100vh - 3em);
		display: flex;
		flex-direction: column;
		margin: 0;
		padding: 0;
	}
	/* The heading stays put; only the list scrolls. */
	#text-table-of-contents {
		min-height: 0;
		overflow-y: auto;
		overscroll-behavior: contain;
		font-size: 90%;
	}
	#text-table-of-contents ul {
		margin: 0;
		padding-left: 1.1em;
	}
	#text-table-of-contents > ul {
		padding-left: 0;
		list-style: none;
	}
	#text-table-of-contents li {
		padding-top: .3em;
	}
}

/* When publishing, s3-publish moves the postamble inside #content so that on
   a wide screen it stays in the content column instead of spanning underneath
   the contents sidebar.  These rules only bite where it has been moved; a
   postamble left as a sibling of #content is untouched. */
div#content > #postamble {
	margin-top: 2em;
	padding-top: 1em;
	border-top: 1px solid #555;
}
/* ...unless the source link sits directly above it, having already drawn the
   separating rule. */
#org-download + #postamble {
	margin-top: .5em;
	padding-top: 0;
	border-top: none;
}
"
  "A self-contained stylesheet for HTML published by s3-publish.
Adapted from the author's simple_dark Emacs HTML theme, with rules
added for the markup that theme does not cover: source block syntax
classes, self-linking headings, the table of contents sidebar, example
blocks, images, the relocated postamble, and the source-file link
s3-publish appends.  Inline code is set smaller than in the source
theme.  Dark only.  See `s3-publish-html-theme'.")




(defcustom s3-publish-html-theme 'default
  "Stylesheet inlined into HTML published from Org and Markdown buffers.

`default'  Embed `s3-publish-html-default-style', a self-contained dark
           theme adapted from the author's simple_dark Emacs HTML theme.
A file     Embed the contents of that CSS file, e.g.
           \"~/.emacs.d/theme/simple_dark.css\".
A string   Embed it verbatim as CSS.  Any multi-line string, or any string
           that does not name a readable file, is treated this way.
nil        Leave your Org HTML export settings alone.  A document is then
           styled only if it carries a stylesheet itself, through
           `#+HTML_HEAD:' or a `#+SETUPFILE:', and Org's minimal default
           stylesheet applies when it does not.

The CSS is inlined, so published pages stay self-contained and no
stylesheet has to be uploaded beside them.  A document that sets its own
`#+HTML_HEAD:' still wins, so per-document themes keep working.

This is also the only way to theme Markdown: it is exported from a
temporary directory, where a document-relative `#+SETUPFILE:' cannot
resolve."
  :type '(choice (const :tag "Built-in s3-publish theme" default)
                 (file :tag "CSS file")
                 (string :tag "CSS")
                 (const :tag "None (use your Org settings)" nil))
  :group 's3-publish)

(defun s3-publish--html-css ()
  "Return the CSS selected by `s3-publish-html-theme'.
Returns nil when no theme is configured."
  (let ((theme s3-publish-html-theme))
    (cond
     ((null theme) nil)
     ((eq theme 'default) s3-publish-html-default-style)
     ((not (stringp theme))
      (error "Invalid `s3-publish-html-theme': %S" theme))
     ;; A single-line string naming a readable file is a path; everything
     ;; else is CSS.  Real CSS spans several lines, so the two never collide.
     ((and (not (string-match-p "\n" theme))
           (file-readable-p (expand-file-name theme)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name theme))
        (buffer-string)))
     (t theme))))

(defconst s3-publish-html-default-favicon
  (concat "data:image/png;base64,"
          "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAIn0lEQVRYw52Xe5BU1RHGf33unTuPnZ19QXYFRJ4GqwyKVGmioBKQoIKPJIVRE2JCUPERyxCLEpNSy1dIRCuVKJZlKqlUlJRJykgAFRCNERQVKZGYIgFBBFweO8vuzs7MfZ3OHzM7u6skouefuTPTp7+v++s+p69wnOvO2S8Nae8MLjVqZygyWYVhotQBqNArlgNWdYs4Zr02ec8+vmrakePxK59mcOOUV06OtXi7qs71nExGUaxGWFXURoRBTBgoGhuMJFAEq+Uiok+nM84DT7x58b8/N4GFZ7/woOukFiXcNH5YwGqI1Qgw+MWYUimisSXJ2C81MmJ8jlxTEmvhaLtlz7YyO99rx7jBst+9M/vHn4nAtTPWjZRStKE+1Tq25HcS2aBiLBDHSleHzxdG1DF7/jimXnoimaxb22sBoxXPbzx3mBX37eVo78FdrUO8ry5df8HeTyWw8Kz1E9WNtmQSzW5vkEdRBBAsYWgo5MtMv/Ik5t99OsaARar/V1DzB8skXKG+JQEYtr92hF/ftAPr9USuJiYv3zxj2/8kcPN5a0aEkdmdchvcUtSpqIpagxGIYkt3PuCan05k1rzRKIKJLTiGrk6fZx/7D++8coiujjJhKWb2gvF844cTMFiW3fC27ni9JE6mGCXq7ehfPX/Rvj5M0/egqiaI5OVMotntDbsUFQGDGLAKXR0B3158KhfOG4MglY2OYdPq/dx6wXpW/WYnHe09YKBUitm6oR2JATWMn9QgPcUuTbtNbtAjLysqnyBwwzlrf55Lto3tDfIYVHSAqN2dPufMHs6cH4xDB2TshT+8z8M3v4GNlea2FOm6JKJKFFqmXzkKcRQEjCNALEU/Ty7ZNvaGs9f+YhCBxTPWjXSc1KJilAcbD9ImDC3ZBo9r7jqtmqrKx5aX2nnirm00NCZJ1zmIhdC3dLSHzFkwjulXjKoR/eBf3TiOgFGKQSeOk1z0o5l/P7FG4GgxusNzMkRRAMbUolSgVAg57+sjaWj0QCudUCpEPL5kK5msSyIjgKG3EFLsDpl3+6l87ycTayXW2x2ybeMhkmkXECLr4zlpir5/B4CZdfWaHOJc5UcFtAraF72NYlzPMOWSEdhaWpS/PLKDzsM+6ToXq4LfG9HcmubOp6Zy2fXjBohk+eND79Gd90kka2rjR0Ww9uqll7xa747Zm5rjGCcb2jJS6eKaYRRCS1uaE8fnKr8qBL5l45p91OWSAMSBxSSEu1dMpX5IchD4plUfse6pvTQ2u4MCizUgIensrqOFOcbGwcUKxDoYHMDGSn2TV9GvmoEdW/N0H/ZJ9GFJxa477w8sETY/386ji9+mLmcQ16mC22rHWRTFsXKxq8gkq2GFnmr1yKF28oWhxUo/tcMfFrGxIlJx6SYMQRSz9NrXmXvLKQwZlualP+9l0+p9pFIuXspU3A5uusp9YjnDFdERVuPK8VnlabBYMTgJw5H9JQp5n1xzJeTGoUkCPyYox8ShxaqSrnMp9UY8tmQrKuAI1OU8HLcWU39qLOCA1RgxdrhR1axqzOBlMHElumJXwLoVe2r/nDGtjTNnDUNVOeXMIUye1oYxCgr1TR71DQmyjR7GqYJja+C2LwlalcFSL9ed/ZzyidVfMnFkCcqWhT+bxJcvHF4Lo7sjItfiAcrBvUXu/e5GentCvKQz2NXA6D/mG8CISEFEQAbaSa0GHNdQ6Ar4/f3bad9TqGUo1+JVNwitI+s4YXSW0LcDtw8AHPi1giVqQOhxVWWf4zoT4shHMP2NWL16jx4qM33uKK57YFKlGwYEsOvdTv7xzIfsfLeTw/uLZOoTn8RUqSbfDMqIcRxiy35XMFuNuhMiQgTFVC9WLBw97DN7/ljmLZnY71CgJx/wxF3v8Oa6A6gFLy2kMh7GVHTvh+t7MoNJAEY8lNLbrvFYLSpXGmtQjaB6v3d3Rpw58wTmLTl1UDJ3vNXBspveoLuj0hmO25/vPvCBxXysZ5FKJYoTrzEn5dIrg6hUcIxXYxoGlky9YcE9p2MxxNWUvr+tk3uv2Ui5N6KpNYXTPwihVonCjx9lg2uhTxlHPCJbKpzUlFtpFq+c0iNinkw62aqBodgb8ZWLhtPQkkSo9HUcWX5561ugkG1IYLVyMzlSkbnUGxFbxVrFOdagJ1ItHSXpZnHEPLl45ZQeA9CQkvuDuIhjPGKrGGDS1NZazVhg7ZO7+WhPgWyDS6xgqu7CSMkfKHHJgvHc96epJDynvxuO0Qyu8QjiItmU3F8TZun6C/bGcXlZ1msGWzmAGtuSg/Zvfy1P0nNABSOVmSbyLYfbfWZ9ZwzfvHkCw0ZnaRuVJfDjKm1bw1Yq82PGayGOy8v6BtSaZMs3zryt29+/K5dsIYpUC50hfYVcMYrx/RirQuRbejoCCl0B37p1AvPvPq2mbyEfYIwMqH6qg61ozmump3xo56MbZ952rNJUr96cX7SdUTrRIK+vPtBXEljg8oVfpOWENIVOH1Vh4jlDuefpc7nilgk1qdY+tZv9u7tJpR0G9oPBaDrRIKUwHyVyDdNE+ie+T47lX10/MSrGW6Rc517/8Dgmnd9ay0S5FLF/Vw/NrWmaWlOD9v3ttztZsfSf1DV4OAkZoLtQ5zVTDDsiSSYmL9/wf8byvrV4xrqRh/LBhow7dOzltwxj+lXDAYvF1FLWd6xs23SYZx7dwfbNR2hs9nATletXxZKQFOlEEz3BwV2Naff4XkwGru9PWvVgUPYWjRwzhFETkwwdmcBLGcrFiPYPenl/WycH9vTiukK6PoExptrnCZJuljAuEcXlZcs3fe2zvZoNXDdOWX1ydyG+3UYy19VURm2MlRgkxk0YEikHxzEYcTC4OCKUtVgUlaczknngoVfP/fwvp4OITN/cEpWPXqaxnWFEJqvRNiBTHTCKYqUdeEtFXnRTjX995MWzOo7H738BS4n5qANJ2+cAAAAASUVORK5CYII=")
  "A 32x32 icon inlined into published pages as a `data:' URI.
Kept small on purpose: it rides along in every page s3-publish publishes.
See `s3-publish-html-favicon'.")

(defconst s3-publish--image-media-types
  '(("png" . "image/png")
    ("ico" . "image/x-icon")
    ("svg" . "image/svg+xml")
    ("gif" . "image/gif")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("webp" . "image/webp")
    ("avif" . "image/avif")
    ("bmp" . "image/bmp"))
  "Media types of the images s3-publish embeds as `data:' URIs.")

(defcustom s3-publish-html-favicon 'default
  "Icon linked from HTML published from Org and Markdown buffers.

`default'  Embed `s3-publish-html-default-favicon'.
A file     Embed that image file, base64 encoded, with its media type
           taken from the extension.
A string   Use it verbatim as the icon link's `href', which lets you point
           at a `data:' URI you built yourself, or at an ordinary URL if
           you would rather link than inline.  Any string that does not
           name a readable file is treated this way.
nil        Add no icon.

A file is inlined rather than linked, for the same reason the stylesheet
is: a published page then stays self-contained, with no icon to upload
beside it and nothing to break when the page is served from another
origin.  The cost is that the icon rides along in every page, so prefer a
small one."
  :type '(choice (const :tag "Built-in icon" default)
                 (file :tag "Image file")
                 (string :tag "href verbatim")
                 (const :tag "None" nil))
  :group 's3-publish)

(defun s3-publish--html-favicon-uri ()
  "Return the `href' for the published page's icon, or nil for no icon."
  (let ((favicon s3-publish-html-favicon))
    (cond
     ((null favicon) nil)
     ((eq favicon 'default) s3-publish-html-default-favicon)
     ((not (stringp favicon))
      (error "Invalid `s3-publish-html-favicon': %S" favicon))
     ;; A readable file is inlined; anything else is already an href.
     ((file-readable-p (expand-file-name favicon))
      (let* ((path (expand-file-name favicon))
             (ext (downcase (or (file-name-extension path) "")))
             (type (or (cdr (assoc ext s3-publish--image-media-types))
                       (error "No known media type for favicon %s" favicon))))
        (concat "data:" type ";base64,"
                (base64-encode-string
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally path)
                   (buffer-string))
                 ;; One line: a data URI cannot carry the newlines
                 ;; `base64-encode-string' inserts by default.
                 t))))
     (t favicon))))

(defcustom s3-publish-html-self-link-headlines t
  "When non-nil, every heading in published HTML links to its own anchor.

Org only gives headings an `id', reachable from the table of contents.
With this enabled each heading's text is a link to that `id', so clicking
a heading anywhere on the page puts its anchor in the address bar, ready
to copy and share.

`s3-publish-html-default-style' styles these links to still read as
headings, revealing themselves only on hover.  A custom
`s3-publish-html-theme' will want to do the same, or headings render as
ordinary links."
  :type 'boolean
  :group 's3-publish)

(defcustom s3-publish-html-copy-button t
  "When non-nil, every code block in published HTML has a copy button.

The button copies the block's contents to the clipboard, leaving out any
line numbers.  It is added by a small inline script, through
`org-html-scripts' rather than the page head, so it is independent of
`s3-publish-html-theme' and survives a document's own `#+HTML_HEAD:'.
A document can still opt out with `#+OPTIONS: html-scripts:nil'.

The script positions the button; its colours come from the theme.
`s3-publish-html-default-style' styles it, and a custom theme can style
`button.s3-publish-copy' to match."
  :type 'boolean
  :group 's3-publish)

(defconst s3-publish-html-copy-button-script "\
<style>
.s3-publish-code { position: relative; }
.s3-publish-copy { position: absolute; top: 0.15em; right: 0.2em; cursor: pointer; }
</style>
<script>
document.addEventListener('DOMContentLoaded', function () {
  function fallbackCopy(text) {
    var area = document.createElement('textarea');
    area.value = text;
    area.setAttribute('readonly', '');
    area.style.position = 'fixed';
    area.style.opacity = '0';
    document.body.appendChild(area);
    area.select();
    var ok = document.execCommand('copy');
    document.body.removeChild(area);
    return ok ? Promise.resolve() : Promise.reject();
  }
  function copy(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text).catch(function () {
        return fallbackCopy(text);
      });
    }
    try { return fallbackCopy(text); } catch (e) { return Promise.reject(e); }
  }
  function blockText(pre) {
    var clone = pre.cloneNode(true);
    clone.querySelectorAll('.linenr').forEach(function (n) { n.remove(); });
    // Org ends every block with a newline; pasted into a shell, it would run.
    return clone.textContent.replace(/\\n$/, '');
  }
  document.querySelectorAll('pre.src, pre.example').forEach(function (pre) {
    var wrap = document.createElement('div');
    wrap.className = 's3-publish-code';
    pre.parentNode.insertBefore(wrap, pre);
    wrap.appendChild(pre);
    var button = document.createElement('button');
    button.type = 'button';
    button.className = 's3-publish-copy';
    button.textContent = 'Copy';
    button.setAttribute('aria-label', 'Copy code to clipboard');
    var timer;
    button.addEventListener('click', function () {
      copy(blockText(pre)).then(function () {
        button.textContent = 'Copied';
      }, function () {
        button.textContent = 'Failed';
      }).then(function () {
        clearTimeout(timer);
        timer = setTimeout(function () { button.textContent = 'Copy'; }, 1500);
      });
    });
    wrap.appendChild(button);
  });
});
</script>
"
  "Markup that adds `s3-publish-html-copy-button' to each code block.")

(defcustom s3-publish-html-ignore-advice '(org-html-src-block)
  "Org HTML export functions whose advice is ignored while publishing.

Advice on Org's exporter, typically added in a personal config to decorate
local exports, otherwise leaks into every published page.  Each function
listed here is exported with its original, unadvised definition; the advice
itself stays in place and applies again as soon as the export finishes.

The default covers source blocks, the usual target of such advice, so the
theme's own code block styling is what readers see.  Set this to nil to
publish with your advice applied."
  :type '(repeat function)
  :group 's3-publish)

(defun s3-publish--call-without-advice (functions fn)
  "Call FN with each of FUNCTIONS temporarily stripped of its advice."
  (if (null functions)
      (funcall fn)
    (let* ((symbol (car functions))
           (advised (and (fboundp symbol) (symbol-function symbol)))
           ;; `advice-add' replaces the definition with a wrapper holding the
           ;; original; `advice--cd*r' unwraps every layer of it.
           (original (and advised (fboundp 'advice--cd*r)
                          (advice--cd*r advised))))
      (if (or (null original) (eq original advised))
          (s3-publish--call-without-advice (cdr functions) fn)
        (cl-letf (((symbol-function symbol) original))
          (s3-publish--call-without-advice (cdr functions) fn))))))

(defun s3-publish--html-src-block-language (text backend _info)
  "Tag the exported source block TEXT with its language, for the theme.

Org only names the language in a class, `src-LANG', which CSS cannot
print.  A `data-lang' attribute can be, via `attr()'.  Applies to the HTML
BACKEND and anything derived from it."
  (if (and (org-export-derived-backend-p backend 'html)
           (string-match "<pre class=\"src src-\\([^\" ]+\\)\"" text))
      (replace-match (format "\\& data-lang=\"%s\"" (match-string 1 text))
                     t nil text)
    text))

(defun s3-publish--call-with-html-export-settings (fn)
  "Call FN with no arguments, with Org's HTML exporter configured.

Applies `s3-publish-html-favicon', `s3-publish-html-theme' and
`s3-publish-html-self-link-headlines'.  Each is independent, and each
binding below is a no-op when its feature is off, so turning all three
off leaves your own Org export settings untouched."
  (let* ((css (s3-publish--html-css))
         (favicon (s3-publish--html-favicon-uri))
         ;; Prepended to `org-html-head', so a document's own `#+HTML_HEAD:'
         ;; still wins and any head the user configured stays in place.
         (extra-head
          (concat
           (if favicon
               (format "<link rel=\"icon\" href=\"%s\" />\n" favicon)
             "")
           (if css
               (concat "<style type=\"text/css\">\n" css "</style>\n")
             ""))))
    (let ((org-html-head
           (if (string= extra-head "")
               org-html-head
             (concat extra-head org-html-head)))
          ;; Our stylesheet replaces Org's, rather than patching over it.
          (org-html-head-include-default-style
           (if css nil org-html-head-include-default-style))
          ;; Org otherwise bakes the running Emacs theme's colours into every
          ;; source block, which fights the page theme.  Emit classes instead
          ;; and let the stylesheet colour them.
          (org-html-htmlize-output-type
           (if css 'css org-html-htmlize-output-type))
          ;; A themed page is a modern page; skip the XHTML 1.0 prologue.
          (org-html-doctype (if css "html5" org-html-doctype))
          (org-html-self-link-headlines
           (if s3-publish-html-self-link-headlines
               t
             org-html-self-link-headlines))
          ;; Org's generated ids are content hashes that change on every
          ;; export, so a shared anchor stops resolving as soon as the
          ;; document is republished.  Prefer a `:CUSTOM_ID:' where the
          ;; document provides one -- Markdown always does, see
          ;; `s3-publish-markdown-to-org' -- which keeps anchors both stable
          ;; across publishes and readable in a URL.
          (org-html-prefer-user-labels
           (if s3-publish-html-self-link-headlines t org-html-prefer-user-labels))
          ;; Lets the theme label each source block with its language.
          (org-export-filter-src-block-functions
           (if css
               (append org-export-filter-src-block-functions
                       '(s3-publish--html-src-block-language))
             org-export-filter-src-block-functions))
          ;; Org's own script slot, which a document's `#+HTML_HEAD:' does
          ;; not replace.  Whatever scripts the user already includes stay.
          (org-html-scripts
           (if s3-publish-html-copy-button
               (concat (if org-html-head-include-scripts
                           (org-element-normalize-string org-html-scripts)
                         "")
                       s3-publish-html-copy-button-script)
             org-html-scripts))
          (org-html-head-include-scripts
           (if s3-publish-html-copy-button t org-html-head-include-scripts)))
      (s3-publish--call-without-advice s3-publish-html-ignore-advice fn))))



;;; Shared publishing core ----------------------------------------------------
;; Both the Org and Markdown publishers go through this, so the two stay
;; feature-for-feature identical.

(defun s3-publish--host-base (profile)
  "Return PROFILE's endpoint host, without protocol or leading bucket label."
  (let* ((bucket (plist-get profile :bucket))
         (endpoint (plist-get profile :endpoint))
         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
         (host-base (replace-regexp-in-string "/$" "" host-base)))
    (if (string-prefix-p (concat bucket ".") host-base)
        (replace-regexp-in-string
         (concat "^" (regexp-quote (concat bucket "."))) "" host-base)
      host-base)))

(defun s3-publish--object-url (profile key)
  "Return the URL for KEY in PROFILE's bucket.
A public https URL when PROFILE has :public-acl, otherwise an s3:// URI."
  (let ((bucket (plist-get profile :bucket)))
    (if (plist-get profile :public-acl)
        (format "https://%s.%s/%s" bucket (s3-publish--host-base profile) key)
      (format "s3://%s/%s" bucket key))))

(defun s3-publish--select-profile ()
  "Prompt for a profile from `s3-publish-profiles' and return it with credentials."
  (s3-publish-get-credentials
   (s3-publish-get-profile (s3-publish--read-profile-name))))

(defun s3-publish--upload-no-feed (file profile key &optional mime-type)
  "Upload FILE to PROFILE's bucket under KEY without touching the RSS feed.
MIME-TYPE, when non-nil, is passed to s3cmd as --mime-type.
Returns the resulting URL (see `s3-publish--object-url')."
  (s3-publish--put profile file key mime-type t)
  (s3-publish--object-url profile key))

(defun s3-publish--publish-document (source-file export-fn title description source-label)
  "Publish SOURCE-FILE and an HTML rendering of it to S3.

SOURCE-FILE is backed up under its own extension.  EXPORT-FN is called
with no arguments and must return the path of a freshly exported HTML
file.  Both objects share one key derived from SOURCE-FILE, differing
only in extension.  A link back to the source, labelled SOURCE-LABEL, is
appended to the bottom of the HTML, and local images are dealt with as
`s3-publish-html-local-images' says.  TITLE and DESCRIPTION feed the RSS
feed and HTML index, where both files appear as a single entry.

Returns the HTML URL, which is also copied to the kill ring."
  (let* ((profile (s3-publish--select-profile))
         (salt (plist-get profile :salt))
         (base-key (file-name-sans-extension
                    (s3-publish-get-file-key source-file salt)))
         (source-key (concat base-key (or (file-name-extension source-file t) "")))
         (html-key (concat base-key ".html"))
         (source-url (s3-publish--object-url profile source-key))
         (html-url (s3-publish--object-url profile html-key))
         (html-file (funcall export-fn)))
    (setq html-file (if (file-symlink-p html-file) (file-truename html-file) html-file))
    ;; Append a link to the source file at the bottom of the content.
    (with-temp-buffer
      (insert-file-contents html-file)
      (let ((footer (format "<div id=\"org-download\"><a href=\"%s\">View %s</a></div>\n"
                            source-url source-label))
            (postamble (save-excursion
                         (goto-char (point-min))
                         (and (re-search-forward "<div id=\"postamble\"" nil t)
                              (match-beginning 0))))
            (content-end (s3-publish--content-end-position)))
        ;; Inside #content, so the link sits with the text on a wide screen
        ;; rather than under the table of contents sidebar.
        (cond
         (postamble (goto-char postamble) (insert footer))
         (content-end (goto-char content-end) (insert footer))
         ((progn (goto-char (point-max)) (search-backward "</body>" nil t))
          (goto-char (match-beginning 0))
          (insert footer))
         (t (goto-char (point-max)) (insert "\n" footer))))
      (write-region (point-min) (point-max) html-file nil 'quiet))
    (let ((images (s3-publish--localize-images
                   html-file (file-name-directory source-file) profile)))
      (s3-publish--transfer-document
       profile
       (append (mapcar (lambda (image) (list (car image) (cdr image) nil))
                       images)
               (list (list source-file source-key "text/plain; charset=utf-8")
                     (list html-file html-key "text/html")))
       html-url title
       (format "%s<p>View: <a href=\"%s\">HTML</a> | <a href=\"%s\">%s</a></p>"
               description html-url source-url source-label)))
    (kill-new html-url)
    (message "%s" html-url)
    html-url))

(defun s3-publish--transfer-document (profile uploads url title description)
  "Upload a document's files to PROFILE's bucket, then list it.
UPLOADS is a list of (FILE KEY MIME-TYPE), MIME-TYPE nil to let s3cmd
guess.  URL, TITLE and DESCRIPTION make the document's entry in the RSS
feed and HTML index, when the profile keeps them.  Returns URL."
  (dolist (upload uploads)
    (s3-publish--put profile (nth 0 upload) (nth 1 upload) (nth 2 upload) t))
  (when (and (plist-get profile :rss-feed)
             (plist-get profile :public-acl))
    (s3-publish-update-feed profile url title description))
  url)

(defun s3-publish--content-end-position ()
  "Return the position just before the `</div>' that closes the #content div.
Nil when the buffer has no #content div.  Org closes it immediately before
the postamble, or before </body> when the document has no postamble."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "<div id=\"content\"" nil t)
      (let ((limit (save-excursion
                     (goto-char (point-min))
                     (cond
                      ((re-search-forward "<div id=\"postamble\"" nil t)
                       (match-beginning 0))
                      ((progn (goto-char (point-min))
                              (re-search-forward "</body>" nil t))
                       (match-beginning 0))
                      (t (point-max))))))
        (goto-char limit)
        (when (search-backward "</div>" nil t)
          (point))))))

(defun s3-publish--move-postamble-into-content (html-file)
  "Move the postamble inside the #content div of HTML-FILE.

Org emits the postamble as a sibling of #content.  On a wide screen,
where the table of contents becomes a column beside the content, a
sibling spans the full width and so lands beneath the sidebar rather than
beneath the text it belongs to.  Inside #content it is a grid item like
everything else and stays in the content column.

Does nothing when the document has no postamble or no #content div."
  (with-temp-buffer
    (insert-file-contents html-file)
    (goto-char (point-min))
    ;; The default postamble holds only <p> elements, so the first </div>
    ;; after it is its own.
    (when (re-search-forward
           "<div id=\"postamble\"\\(?:.\\|\n\\)*?</div>[ \t]*\n?" nil t)
      ;; `string-trim' runs its own regexp match, which would clobber the
      ;; match data `replace-match' needs, so cut first and trim after.
      (let ((postamble (match-string 0)))
        (replace-match "" t t)
        (setq postamble (string-trim postamble))
        (let ((end (s3-publish--content-end-position)))
          (when end
            (goto-char end)
            (insert postamble "\n")
            (write-region (point-min) (point-max) html-file nil 'quiet))))))
  html-file)

(defun s3-publish--rewrite-toc-heading (html-file)
  "Retitle the table of contents in HTML-FILE with the document's own title.

Org heads the contents list \"Table of Contents\", which tells a reader
nothing the list does not already show.  The document's first header --
its title -- is more use there, and makes the entries read as nested
beneath it.  The heading links back to the top of the document.

When the document has no title the heading is dropped rather than
replaced.  Does nothing when the document was exported without a table of
contents."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let ((title nil)
          (modified nil))
      (goto-char (point-min))
      (when (re-search-forward
             "<h1 class=\"title\">\\(\\(?:.\\|\n\\)*?\\)</h1>" nil t)
        (setq title (match-string 1)))
      (goto-char (point-min))
      (when (and (re-search-forward "<div id=\"table-of-contents\"[^>]*>" nil t)
                 (re-search-forward "[ \t\n]*<h2>\\(?:.\\|\n\\)*?</h2>\n?" nil t))
        (replace-match
         (if title
             (format "\n<h2 class=\"toc-title\"><a href=\"#content\">%s</a></h2>\n"
                     title)
           "\n")
         t t)
        (setq modified t))
      (when modified
        (write-region (point-min) (point-max) html-file nil 'quiet))))
  html-file)

(defcustom s3-publish-html-local-images 'inline
  "How published HTML carries images that live on your own disk.

An Org link like [[file:diagram.png]], or Markdown's ![](shot.png),
exports to an <img> pointing at a local file, which nobody else can
see.  So when publishing an Org or Markdown document:

`inline'  Embed each local image in the page as a `data:' URI, keeping
          the page self-contained.  An image larger than
          `s3-publish-html-inline-image-max-size' is uploaded instead,
          as with `upload'.
`upload'  Upload each local image beside the page and link to it.  The
          image's key is derived from its own path, like any published
          file, so republishing reuses it rather than adding another.
nil       Leave image links as Org wrote them.

Images on the web (http:, https:, data: and so on) are never touched.
A link to an image that cannot be read is left alone with a warning."
  :type '(choice (const :tag "Embed in the page" inline)
                 (const :tag "Upload beside the page" upload)
                 (const :tag "Leave alone" nil))
  :group 's3-publish)

(defcustom s3-publish-html-inline-image-max-size (* 1024 1024)
  "Largest local image, in bytes, that is embedded in a published page.
Base64 makes an embedded image a third larger, and it is downloaded with
the page every time, so larger images are uploaded beside the page
instead.  Only applies when `s3-publish-html-local-images' is `inline'."
  :type 'integer
  :group 's3-publish)

(defun s3-publish--local-image-file (src directory)
  "Return the local file an <img> SRC names, relative to DIRECTORY.
Returns nil when SRC is a URL rather than a local file."
  (let ((src (s3-publish--html-unescape src)))
    (cond
     ((string-match "\\`file://\\(/.*\\)" src)
      (url-unhex-string (match-string 1 src)))
     ((string-match "\\`file:\\(.*\\)" src)
      (expand-file-name (url-unhex-string (match-string 1 src)) directory))
     ;; Any other scheme, data: included, is not ours to rewrite.
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" src) nil)
     ((string-prefix-p "//" src) nil)
     (t (expand-file-name (url-unhex-string src) directory)))))

(defun s3-publish--html-unescape (text)
  "Undo the HTML escaping of the few characters an attribute escapes."
  (let ((case-fold-search nil))
    (dolist (pair '(("&quot;" . "\"") ("&lt;" . "<") ("&gt;" . ">")
                    ("&#39;" . "'") ("&amp;" . "&"))
                  text)
      (setq text (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) text t t)))))

(defun s3-publish--file-data-uri (file)
  "Return FILE as a base64 `data:' URI, or nil for an unknown image type."
  (let ((type (cdr (assoc (downcase (or (file-name-extension file) ""))
                          s3-publish--image-media-types))))
    (when type
      (concat "data:" type ";base64,"
              (base64-encode-string
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (buffer-string))
               ;; A data URI cannot carry the line breaks added by default.
               t)))))

(defun s3-publish--localize-images (html-file directory profile)
  "Rewrite the local images in HTML-FILE so they work once published.
Relative image links are resolved against DIRECTORY, the directory of
the document's source.  See `s3-publish-html-local-images' for what is
done to them; PROFILE gives the URL of an uploaded image.

Returns the images to upload, as a list of (FILE . KEY)."
  (let ((uploads nil))
    (when s3-publish-html-local-images
      (with-temp-buffer
        (insert-file-contents html-file)
        (goto-char (point-min))
        (let ((modified nil)
              (case-fold-search t))
          (while (re-search-forward "<img[^>]*?[ \t\n]src=\"\\([^\"]*\\)\"" nil t)
            (let* ((src (match-string 1))
                   (file (s3-publish--local-image-file src directory)))
              (cond
               ((null file))
               ((not (file-readable-p file))
                (display-warning 's3-publish
                                 (format "Cannot read image %s; left as is" file)))
               (t
                (let ((uri (and (eq s3-publish-html-local-images 'inline)
                                (<= (file-attribute-size (file-attributes file))
                                    s3-publish-html-inline-image-max-size)
                                (s3-publish--file-data-uri file))))
                  (unless uri
                    (let ((key (s3-publish-get-file-key
                                file (plist-get profile :salt))))
                      (unless (assoc file uploads)
                        (push (cons file key) uploads))
                      (setq uri (s3-publish--object-url profile key))))
                  (replace-match uri t t nil 1)
                  (setq modified t))))))
          (when modified
            (write-region (point-min) (point-max) html-file nil 'quiet)))))
    (nreverse uploads)))

(defun s3-publish--export-buffer-to-html ()
  "Export the current Org buffer to HTML and return an absolute file name.
The export is themed according to `s3-publish-html-theme'.  Both the Org
and the Markdown publisher export through here, so the two are styled
identically.

`org-html-export-to-html' returns a name relative to the Org buffer's
`default-directory', which stops resolving once another buffer is current,
so it is expanded here while that buffer is still active."
  (s3-publish--call-with-html-export-settings
   (lambda ()
     (let* ((html-file (expand-file-name (org-html-export-to-html)))
            (html-file (if (file-symlink-p html-file)
                           (file-truename html-file)
                         html-file)))
       (s3-publish--rewrite-toc-heading html-file)
       (s3-publish--move-postamble-into-content html-file)))))

(defun s3-publish--org-string-export-to-html (org-text)
  "Export ORG-TEXT, a string of Org markup, to a temporary HTML file.
Returns the path of the exported file.  Uses Org's own HTML exporter, so
the result is styled exactly like any other Org export."
  (let* ((tmp-dir (make-temp-file "s3-publish-export-" t))
         (org-file (expand-file-name "export.org" tmp-dir))
         (buffer nil))
    (with-temp-file org-file (insert org-text))
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect org-file))
          (with-current-buffer buffer
            (s3-publish--export-buffer-to-html)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))


;;; Org publishing ------------------------------------------------------------

(defcustom s3-publish-org-tangle nil
  "Whether `s3-publish-org-buffer' tangles the buffer before exporting it.

Literate documents often evaluate code that runs the files they tangle,
so exporting without tangling first runs stale or missing files.
Tangling writes those files, overwriting any changes made to them
directly, so it is off by default.

  nil   Never tangle.  A document asking for it with
        `#+S3_PUBLISH_TANGLE: t' is an error rather than being
        published from stale files.
  ask   Ask before tangling a document that has blocks to tangle.

Tangling always asks first; it never happens implicitly.  A document
can skip the question with `#+S3_PUBLISH_TANGLE: nil', which is never
tangled.  `#+S3_PUBLISH_TANGLE: t' marks a document that needs
tangling: it is still asked about, but is an error when this is nil.

Only `s3-publish-org-buffer' tangles.  Regions and Markdown are exported
from a temporary copy, where relative tangle paths would point at the
wrong place."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" ask))
  :group 's3-publish)

(defun s3-publish--org-tangle-keyword ()
  "Return the current buffer's `#+S3_PUBLISH_TANGLE:' setting.
Returns t or nil when the keyword is set, and the symbol `unset' when
it is absent."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (not (re-search-forward
                "^[ \t]*#\\+S3_PUBLISH_TANGLE:[ \t]*\\(.*?\\)[ \t]*$" nil t))
          'unset
        (let ((value (downcase (match-string-no-properties 1))))
          (cond
           ((member value '("t" "yes")) t)
           ((member value '("nil" "no")) nil)
           (t (user-error "Invalid #+S3_PUBLISH_TANGLE: %S (use t or nil)"
                          (match-string-no-properties 1)))))))))

(defun s3-publish--org-has-tangle-blocks-p ()
  "Return non-nil when the current Org buffer has a source block to tangle.
Header arguments are resolved as Org resolves them, so a `:tangle' set
through `#+PROPERTY:' or a heading's properties counts."
  (require 'ob-core)
  (let (found)
    (org-babel-map-src-blocks nil
      (unless found
        (let ((tangle (cdr (assq :tangle
                                 (nth 2 (org-babel-get-src-block-info 'no-eval))))))
          (when (and tangle (not (equal tangle "no")))
            (setq found t)))))
    found))

(defun s3-publish--org-maybe-tangle ()
  "Tangle the current Org buffer if `s3-publish-org-tangle' says to.
See that variable for how it combines with `#+S3_PUBLISH_TANGLE:'.
Returns non-nil when the buffer was tangled."
  (let ((document (s3-publish--org-tangle-keyword)))
    (when (and (eq document t) (null s3-publish-org-tangle))
      (user-error "This document sets #+S3_PUBLISH_TANGLE: t, \
but `s3-publish-org-tangle' is nil"))
    (when (and s3-publish-org-tangle
               document
               (s3-publish--org-has-tangle-blocks-p)
               (y-or-n-p "Tangle this document before publishing? "))
      (require 'ob-tangle)
      (org-babel-tangle)
      t)))

(defun s3-publish-org-buffer ()
  "Export the current Org buffer to HTML and upload it to S3.
Before exporting, save the buffer and back up the original .org file to S3.
Both files share the same key (computed from the org file and profile salt)
with only their extensions differing.
A link to the .org file is appended to the bottom of the HTML file.
Only the HTML file URL is displayed and copied to the kill ring.
When updating the RSS feed, both files are grouped as a single entry.
The buffer is tangled first when `s3-publish-org-tangle' says to."
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))
  (let ((org-file (buffer-file-name)))
    (unless org-file
      (error "Current org buffer is not visiting a file"))
    (s3-publish--org-maybe-tangle)
    (let ((title (or (save-excursion
                       (goto-char (point-min))
                       (and (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
                            (match-string 1)))
                     (file-name-nondirectory org-file)))
          (description (or (save-excursion
                             (goto-char (point-min))
                             (and (re-search-forward "^#\\+DESCRIPTION:\\s-*\\(.*\\)$" nil t)
                                  (match-string 1)))
                           (format "Published on %s"
                                   (format-time-string "%Y-%m-%d %H:%M"))))
          (buffer (current-buffer)))
      (s3-publish--publish-document
       org-file
       (lambda () (with-current-buffer buffer (s3-publish--export-buffer-to-html)))
       title description "Org source"))))


;;; Markdown -> Org conversion ------------------------------------------------

(defcustom s3-publish-markdown-to-org-command nil
  "Optional shell command converting Markdown to Org markup.

When nil (the default) the built-in converter
`s3-publish-markdown-to-org' is used, which needs no external programs.
When set to a string, that command is run with the Markdown text on
standard input and must write Org markup to standard output, e.g.

  \"pandoc -f gfm -t org\"

Either way the resulting Org markup is rendered by Org's own HTML
exporter, so published Markdown looks identical to published Org."
  :type '(choice (const :tag "Built-in converter" nil)
                 (string :tag "Shell command"))
  :group 's3-publish)

(defconst s3-publish--md-hr-regexp
  "^[ \t]*\\(\\(-[ \t]*\\)\\{3,\\}\\|\\(\\*[ \t]*\\)\\{3,\\}\\|\\(_[ \t]*\\)\\{3,\\}\\)$"
  "Regexp matching a Markdown thematic break.")

;; Sentinel characters standing in for Org emphasis markers while inline
;; conversion runs, so that freshly written markers are never re-scanned.
(defconst s3-publish--md-bold-open (string 1))
(defconst s3-publish--md-bold-close (string 2))
(defconst s3-publish--md-ital-open (string 3))
(defconst s3-publish--md-ital-close (string 4))

(defun s3-publish--md-fence (line)
  "If LINE opens or closes a fenced code block, return (TOKEN . LANG).
TOKEN is the run of backticks or tildes, LANG the info string."
  (when (string-match "^[ \t]*\\(```+\\|~~~+\\)[ \t]*\\([^ \t`~]*\\)" line)
    (cons (match-string 1 line) (match-string 2 line))))

(defun s3-publish--md-fence-close-p (line token)
  "Return non-nil if LINE closes a fenced block opened with TOKEN."
  (string-match-p (concat "^[ \t]*" (regexp-quote token) "[ \t]*$") line))

(defun s3-publish--md-unquote (string)
  "Trim STRING and strip one layer of surrounding single or double quotes."
  (let ((s (string-trim string)))
    (if (and (> (length s) 1)
             (or (and (string-prefix-p "\"" s) (string-suffix-p "\"" s))
                 (and (string-prefix-p "'" s) (string-suffix-p "'" s))))
        (substring s 1 -1)
      s)))

(defun s3-publish--md-inline-to-org (text)
  "Convert inline Markdown markup in TEXT to Org markup.
Code spans and backslash escapes are protected from the other passes."
  (with-temp-buffer
    (insert text)
    (let ((protected '())
          (count 0))
      (cl-flet ((protect (regexp group kind)
                  ;; Replace each match of REGEXP with a placeholder, stashing
                  ;; GROUP's text together with KIND for later restoration.
                  (goto-char (point-min))
                  (while (re-search-forward regexp nil t)
                    (push (cons kind (match-string group)) protected)
                    (replace-match (format "%c%d%c" 0 count 0) t t)
                    (setq count (1+ count))))
                (rewrite (regexp replacement)
                  (goto-char (point-min))
                  (while (re-search-forward regexp nil t)
                    (replace-match replacement t))))
        ;; 1. Protect backslash escapes and inline code spans.
        (protect "\\\\\\([][*_`~()#+.!>|-]\\)" 1 'raw)
        (protect "``\\([^\n]+?\\)``" 1 'code)
        (protect "`\\([^`\n]+?\\)`" 1 'code)
        ;; 2. Images, links, autolinks.  Images first: ![alt](url) vs [t](url).
        (rewrite "!\\[\\([^][]*\\)\\]([ \t]*\\([^) \t]+\\)[^)]*)" "[[\\2]]")
        (rewrite "\\[\\([^][]*\\)\\]([ \t]*\\([^) \t]+\\)[^)]*)" "[[\\2][\\1]]")
        ;; A target with no scheme is a relative file, but Org reads
        ;; `[[shot.png]]' as a search for a heading of that name, which
        ;; aborts the export.  Say it is a file.  `#anchor' stays as is.
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\([^]\n]+\\)\\]" nil t)
          (let ((target (match-string 1)))
            (unless (string-match-p "\\`\\(?:#\\|[a-zA-Z][a-zA-Z0-9+.-]*:\\)"
                                    target)
              (replace-match (concat "file:" target) t t nil 1))))
        (rewrite "<\\(\\(?:https?\\|mailto\\):[^> \t]+\\)>" "[[\\1]]")
        ;; 3. Footnote references and definitions.
        (rewrite "^\\[\\^\\([^]]+\\)\\]:[ \t]*" "[fn:\\1] ")
        (rewrite "\\[\\^\\([^]]+\\)\\]" "[fn:\\1]")
        ;; 4. Emphasis, widest markers first, writing sentinels not markers.
        (rewrite "\\*\\*\\*\\([^*\n]+?\\)\\*\\*\\*"
                 (concat s3-publish--md-bold-open s3-publish--md-ital-open
                         "\\1"
                         s3-publish--md-ital-close s3-publish--md-bold-close))
        (rewrite "\\*\\*\\([^*\n]+?\\)\\*\\*"
                 (concat s3-publish--md-bold-open "\\1" s3-publish--md-bold-close))
        (rewrite "___\\([^_\n]+?\\)___"
                 (concat s3-publish--md-bold-open s3-publish--md-ital-open
                         "\\1"
                         s3-publish--md-ital-close s3-publish--md-bold-close))
        (rewrite "__\\([^_\n]+?\\)__"
                 (concat s3-publish--md-bold-open "\\1" s3-publish--md-bold-close))
        (rewrite "~~\\([^~\n]+?\\)~~" "+\\1+")
        (rewrite "\\(^\\|[^*[:alnum:]]\\)\\*\\([^*\n]+?\\)\\*"
                 (concat "\\1" s3-publish--md-ital-open "\\2" s3-publish--md-ital-close))
        (rewrite "\\(^\\|[^[:alnum:]_]\\)_\\([^_\n]+?\\)_\\($\\|[^[:alnum:]_]\\)"
                 (concat "\\1" s3-publish--md-ital-open "\\2"
                         s3-publish--md-ital-close "\\3"))
        ;; 5. Turn sentinels into real Org emphasis markers.
        (rewrite (regexp-quote s3-publish--md-bold-open) "*")
        (rewrite (regexp-quote s3-publish--md-bold-close) "*")
        (rewrite (regexp-quote s3-publish--md-ital-open) "/")
        (rewrite (regexp-quote s3-publish--md-ital-close) "/")
        ;; 6. Restore protected spans.
        (setq protected (nreverse protected))
        (dotimes (i count)
          (let ((entry (nth i protected)))
            (goto-char (point-min))
            (when (re-search-forward (format "%c%d%c" 0 i 0) nil t)
              (replace-match (if (eq (car entry) 'code)
                                 (concat "=" (cdr entry) "=")
                               (cdr entry))
                             t t))))))
    (buffer-string)))

(defun s3-publish--md-strip-frontmatter (lines)
  "Strip YAML front matter from LINES.
Returns a list (REMAINING-LINES TITLE DESCRIPTION); TITLE and DESCRIPTION
are nil when absent.  LINES is returned unchanged if the block never closes."
  (if (and lines (string-match-p "^---[ \t]*$" (car lines)))
      (let ((rest (cdr lines))
            (closed nil)
            title description)
        (while (and rest (not closed))
          (let ((line (car rest)))
            (setq rest (cdr rest))
            (cond
             ((string-match-p "^\\(---\\|\\.\\.\\.\\)[ \t]*$" line)
              (setq closed t))
             ((string-match "^title:[ \t]*\\(.*\\)$" line)
              (setq title (s3-publish--md-unquote (match-string 1 line))))
             ((string-match "^\\(description\\|summary\\):[ \t]*\\(.*\\)$" line)
              (setq description (s3-publish--md-unquote (match-string 2 line)))))))
        (if closed (list rest title description) (list lines nil nil)))
    (list lines nil nil)))

(defun s3-publish--md-normalize-setext (lines)
  "Rewrite setext headings in LINES into ATX form, leaving code fences alone."
  (let ((out '())
        (in-fence nil)
        (token nil))
    (while lines
      (let ((line (car lines))
            (next (cadr lines)))
        (cond
         (in-fence
          (when (s3-publish--md-fence-close-p line token)
            (setq in-fence nil token nil))
          (push line out)
          (setq lines (cdr lines)))
         ((s3-publish--md-fence line)
          (setq token (car (s3-publish--md-fence line)) in-fence t)
          (push line out)
          (setq lines (cdr lines)))
         ((and next
               (not (string-match-p "^[ \t]*$" line))
               (not (string-match-p "^[ \t]*[#>|]" line))
               (not (string-match-p "^[ \t]*[-*+][ \t]" line))
               (string-match-p "^=\\{2,\\}[ \t]*$" next))
          (push (concat "# " (string-trim line)) out)
          (setq lines (cddr lines)))
         ((and next
               (not (string-match-p "^[ \t]*$" line))
               (not (string-match-p "^[ \t]*[#>|]" line))
               (not (string-match-p "^[ \t]*[-*+][ \t]" line))
               (string-match-p "^-\\{2,\\}[ \t]*$" next))
          (push (concat "## " (string-trim line)) out)
          (setq lines (cddr lines)))
         (t
          (push line out)
          (setq lines (cdr lines))))))
    (nreverse out)))

(defun s3-publish--md-atx-level (line)
  "Return the ATX heading level of LINE, or nil if LINE is not a heading."
  (when (string-match "^\\(#\\{1,6\\}\\)[ \t]+" line)
    (length (match-string 1 line))))

(defun s3-publish--md-count-h1 (lines)
  "Count level-1 ATX headings in LINES, ignoring fenced code blocks."
  (let ((count 0) (in-fence nil) (token nil))
    (dolist (line lines)
      (cond
       (in-fence
        (when (s3-publish--md-fence-close-p line token)
          (setq in-fence nil token nil)))
       ((s3-publish--md-fence line)
        (setq token (car (s3-publish--md-fence line)) in-fence t))
       ((eq (s3-publish--md-atx-level line) 1)
        (setq count (1+ count)))))
    count))

(defun s3-publish--md-table-rule (row)
  "Return an Org table rule row matching the column count of ROW."
  (let ((cells (split-string (string-trim row "[ \t|]+" "[ \t|]+") "|")))
    (concat "|" (mapconcat (lambda (_) "---") cells "+") "|")))

(defun s3-publish--md-table-separator-p (line)
  "Return non-nil if LINE is a Markdown table header separator."
  (and (string-match-p "^[ \t]*|" line)
       (string-match-p "^[ \t]*|[ \t:|-]*-[ \t:|-]*$" line)))

(defvar s3-publish--md-slugs nil
  "Anchor slugs already used while converting one Markdown document.
A hash table, bound by `s3-publish-markdown-to-org'.  Nil outside a
conversion, in which case slugs are not uniquified.")

(defun s3-publish--md-slug (text)
  "Return an anchor slug for Markdown heading TEXT.

Slugs are lowercase and dash separated, so a published anchor reads as
part of the URL.  Within one document they are uniquified with a numeric
suffix, since two headings may share a name."
  (let* ((plain text)
         ;; Keep a link's text and drop its target, or the URL lands in the
         ;; slug.
         (plain (replace-regexp-in-string "!?\\[\\([^][]*\\)\\]([^)]*)" "\\1" plain))
         (plain (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase plain)))
         (base (replace-regexp-in-string "\\(?:^-+\\|-+$\\)" "" plain))
         (base (if (string= base "") "section" base))
         (slug base)
         (n 1))
    (when s3-publish--md-slugs
      (while (gethash slug s3-publish--md-slugs)
        (setq n (1+ n))
        (setq slug (format "%s-%d" base n)))
      (puthash slug t s3-publish--md-slugs))
    slug))

(defun s3-publish--md-convert-lines (lines shift)
  "Convert Markdown LINES to a list of Org lines.
SHIFT is subtracted from every ATX heading level; headings that fall below
level 1 are dropped, which is how a promoted title is removed from the body."
  (let ((out '())
        (in-fence nil)
        (token nil)
        (lang nil))
    (while lines
      (let ((line (car lines)))
        (setq lines (cdr lines))
        (cond
         ;; Inside a fenced code block: pass through verbatim, escaping lines
         ;; that Org would otherwise read as markup.
         (in-fence
          (if (s3-publish--md-fence-close-p line token)
              (progn
                (push (if (string= lang "") "#+end_example" "#+end_src") out)
                (setq in-fence nil token nil lang nil))
            (push (if (string-match-p "^[*,]\\|^#\\+" line) (concat "," line) line)
                  out)))
         ;; Opening fence.
         ((s3-publish--md-fence line)
          (let ((fence (s3-publish--md-fence line)))
            (setq token (car fence) lang (cdr fence) in-fence t)
            (push (if (string= lang "")
                      "#+begin_example"
                    (format "#+begin_src %s" lang))
                  out)))
         ;; Thematic break.  Checked before lists so that "***" and "---"
         ;; are not mistaken for bullets.
         ((string-match-p s3-publish--md-hr-regexp line)
          (push "-----" out))
         ;; ATX heading.
         ((s3-publish--md-atx-level line)
          (string-match "^\\(#\\{1,6\\}\\)[ \t]+\\(.*?\\)[ \t]*#*[ \t]*$" line)
          (let ((level (- (length (match-string 1 line)) shift))
                (text (match-string 2 line)))
            (when (>= level 1)
              (push (concat (make-string level ?*) " "
                            (s3-publish--md-inline-to-org text))
                    out)
              ;; A CUSTOM_ID gives the heading a stable, readable anchor.
              ;; Org's own ids are content hashes that change on every
              ;; export, so a shared link stops resolving as soon as the
              ;; document is republished.  See `org-html-prefer-user-labels'.
              (push ":PROPERTIES:" out)
              (push (concat ":CUSTOM_ID: " (s3-publish--md-slug text)) out)
              (push ":END:" out))))
         ;; Blockquote: gather the run, convert its contents, wrap it.
         ((string-match-p "^[ \t]*>" line)
          (let ((quoted '()))
            (push (replace-regexp-in-string "^[ \t]*>[ \t]?" "" line) quoted)
            (while (and lines (string-match-p "^[ \t]*>" (car lines)))
              (push (replace-regexp-in-string "^[ \t]*>[ \t]?" "" (car lines)) quoted)
              (setq lines (cdr lines)))
            (push "#+begin_quote" out)
            (dolist (inner (s3-publish--md-convert-lines (nreverse quoted) 0))
              (push inner out))
            (push "#+end_quote" out)))
         ;; Table rows.
         ((s3-publish--md-table-separator-p line)
          (push (s3-publish--md-table-rule line) out))
         ((string-match-p "^[ \t]*|" line)
          (push (s3-publish--md-inline-to-org (string-trim-right line)) out))
         ;; Unordered list item.  Org reads "*" at column 0 as a heading, so
         ;; every bullet is normalised to "-".
         ((string-match "^\\([ \t]*\\)[-*+][ \t]+\\(.*\\)$" line)
          (let ((indent (match-string 1 line))
                (body (match-string 2 line)))
            (setq body (replace-regexp-in-string "^\\[[xX]\\][ \t]+" "[X] " body))
            (setq body (replace-regexp-in-string "^\\[ \\][ \t]+" "[ ] " body))
            (push (concat indent "- " (s3-publish--md-inline-to-org body)) out)))
         ;; Ordered list item.
         ((string-match "^\\([ \t]*\\)\\([0-9]+\\)[.)][ \t]+\\(.*\\)$" line)
          (push (concat (match-string 1 line) (match-string 2 line) ". "
                        (s3-publish--md-inline-to-org (match-string 3 line)))
                out))
         ;; Blank line.
         ((string-match-p "^[ \t]*$" line)
          (push "" out))
         ;; Ordinary text.
         (t
          (let ((converted (s3-publish--md-inline-to-org line)))
            ;; Keep leading markup from being read as Org structure.
            (cond
             ((string-prefix-p "*" converted) (setq converted (concat " " converted)))
             ((string-prefix-p "#+" converted) (setq converted (concat "," converted))))
            (push converted out))))))
    ;; An unterminated fence still needs closing or Org swallows the rest.
    (when in-fence
      (push (if (string= lang "") "#+end_example" "#+end_src") out))
    (nreverse out)))

(defun s3-publish--md-heading-text (line)
  "Return the text of ATX heading LINE, or nil if LINE is not one."
  (when (string-match "^#\\{1,6\\}[ \t]+\\(.*?\\)[ \t]*#*[ \t]*$" line)
    (match-string 1 line)))

(defun s3-publish--md-promote-title (lines)
  "Promote a level-1 heading in LINES to the document title.

Returns a cons (TITLE . REMAINING-LINES).  TITLE is nil when no heading
qualifies, in which case LINES comes back unchanged.

A level-1 heading becomes the title when it is the document's first
content -- the conventional shape for Markdown, and what almost all
generated Markdown looks like -- or when it is the only one in the
document.

Any later level-1 heading repeating that exact text is dropped too.  A
heading identical to the title renders the title a second time and shows
up again in the contents, which is noise rather than structure; its body
is kept."
  (let ((index 0)
        (in-fence nil)
        (token nil)
        (first-content nil)
        (h1-indices '()))
    (dolist (line lines)
      (cond
       (in-fence
        (when (s3-publish--md-fence-close-p line token)
          (setq in-fence nil token nil)))
       ((s3-publish--md-fence line)
        (setq token (car (s3-publish--md-fence line)) in-fence t))
       (t
        (when (and (null first-content)
                   (not (string-match-p "^[ \t]*$" line)))
          (setq first-content index))
        (when (eq (s3-publish--md-atx-level line) 1)
          (push index h1-indices))))
      (setq index (1+ index)))
    (setq h1-indices (nreverse h1-indices))
    (let ((first-h1 (car h1-indices)))
      (if (not (and first-h1
                    (or (eql first-h1 first-content)
                        (= (length h1-indices) 1))))
          (cons nil lines)
        (let ((title (s3-publish--md-heading-text (nth first-h1 lines)))
              (keep '())
              (i 0)
              (drop-blank nil))
          (dolist (line lines)
            (cond
             ((or (eql i first-h1)
                  (and (memq i h1-indices)
                       (equal (s3-publish--md-heading-text line) title)))
              ;; Take the blank line that followed the heading with it, so
              ;; removing it does not leave a gap in the text.
              (setq drop-blank t))
             ((and drop-blank (string-match-p "^[ \t]*$" line))
              (setq drop-blank nil))
             (t
              (setq drop-blank nil)
              (push line keep)))
            (setq i (1+ i)))
          (cons title (nreverse keep)))))))

(defun s3-publish-markdown-to-org (markdown)
  "Convert MARKDOWN text to Org markup and return it as a string.

Handles ATX and setext headings, YAML front matter, emphasis, inline
code, fenced code blocks, ordered/unordered/nested/task lists, links,
images, autolinks, blockquotes, tables, thematic breaks, footnotes and
backslash escapes.

A document whose body contains exactly one level-1 heading and no front
matter title has that heading promoted to #+TITLE and its remaining
headings shifted up one level, so Markdown written in the usual
single-H1 style exports with a document title rather than a numbered
first section.

Indented (four space) code blocks and raw inline HTML are passed through
as ordinary text rather than converted."
  (let* ((lines (split-string markdown "\n"))
         ;; A document's final newline is not a blank content line.
         (lines (if (and lines (equal (car (last lines)) ""))
                    (butlast lines)
                  lines))
         (frontmatter (s3-publish--md-strip-frontmatter lines))
         (lines (s3-publish--md-normalize-setext (nth 0 frontmatter)))
         (description (nth 2 frontmatter))
         ;; A title in the front matter wins, and leaves the body's headings
         ;; alone; otherwise a level-1 heading may be promoted out of it.
         (promoted (if (nth 1 frontmatter)
                       (cons nil lines)
                     (s3-publish--md-promote-title lines)))
         (title (or (nth 1 frontmatter) (car promoted)))
         (body (cdr promoted))
         ;; Shift the remaining headings up only when the promotion left no
         ;; level-1 heading behind.  Shifting with one still present would
         ;; drop it, or flatten it against its own children.
         (shift (if (and (car promoted) (= (s3-publish--md-count-h1 body) 0))
                    1
                  0)))
    (concat
     (if title
         (format "#+TITLE: %s\n" (s3-publish--md-inline-to-org title))
       "")
     (if description
         (format "#+DESCRIPTION: %s\n" (s3-publish--md-inline-to-org description))
       "")
     ;; Markdown uses _ and ^ as ordinary characters; stop Org's exporter
     ;; from reading snake_case and a^b as subscripts and superscripts.
     "#+OPTIONS: ^:nil\n\n"
     (let ((s3-publish--md-slugs (make-hash-table :test 'equal)))
       (mapconcat #'identity (s3-publish--md-convert-lines body shift) "\n"))
     "\n")))

(defun s3-publish--markdown-to-org-string (markdown)
  "Convert MARKDOWN to Org markup, honouring `s3-publish-markdown-to-org-command'."
  (if (null s3-publish-markdown-to-org-command)
      (s3-publish-markdown-to-org markdown)
    (with-temp-buffer
      (insert markdown)
      (let ((exit-code (shell-command-on-region
                        (point-min) (point-max)
                        s3-publish-markdown-to-org-command
                        nil t "*s3-publish-markdown-error*" t)))
        (unless (and (integerp exit-code) (zerop exit-code))
          (error "Markdown conversion command failed: %s"
                 s3-publish-markdown-to-org-command))
        (buffer-string)))))


;;; Markdown publishing -------------------------------------------------------

(defun s3-publish--markdown-buffer-p ()
  "Return non-nil if the current buffer holds Markdown.
Recognises `markdown-mode' and derivatives such as `gfm-mode', and falls
back to the visited file's extension so a .md file works even in
`fundamental-mode'."
  (or (derived-mode-p 'markdown-mode)
      (and buffer-file-name
           (member (downcase (or (file-name-extension buffer-file-name) ""))
                   '("md" "markdown" "mdown" "mkd" "mkdn")))))

(defun s3-publish-markdown-buffer ()
  "Export the current Markdown buffer to HTML and upload it to S3.
Before exporting, save the buffer and back up the original .md file to S3.
Both files share the same key (computed from the markdown file and profile
salt) with only their extensions differing.
A link to the .md file is appended to the bottom of the HTML file.
Only the HTML file URL is displayed and copied to the kill ring.
When updating the RSS feed, both files are grouped as a single entry.

The Markdown is converted to Org (see `s3-publish-markdown-to-org-command')
and rendered with Org's HTML exporter, so the result is styled identically
to a published Org buffer."
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))
  (let ((md-file (buffer-file-name)))
    (unless md-file
      (error "Current markdown buffer is not visiting a file"))
    (let* ((markdown (buffer-substring-no-properties (point-min) (point-max)))
           (org-text (s3-publish--markdown-to-org-string markdown))
           (title (or (and (string-match "^#\\+TITLE:[ \t]*\\(.*\\)$" org-text)
                           (match-string 1 org-text))
                      (file-name-nondirectory md-file)))
           (description (or (and (string-match "^#\\+DESCRIPTION:[ \t]*\\(.*\\)$" org-text)
                                 (match-string 1 org-text))
                            (format "Published on %s"
                                    (format-time-string "%Y-%m-%d %H:%M")))))
      (s3-publish--publish-document
       md-file
       (lambda () (s3-publish--org-string-export-to-html org-text))
       title description "Markdown source"))))


;;; Removal of published documents --------------------------------------------

(defun s3-publish--delete-key (profile key &optional noerror)
  "Delete KEY from PROFILE's bucket, returning t on success.
With NOERROR, return nil on failure instead of signalling an error."
  (let ((result (s3-publish--s3cmd
                 profile "del"
                 (format "s3://%s/%s" (plist-get profile :bucket) key))))
    (cond
     ((zerop (car result)) t)
     (noerror nil)
     (t (error "Error removing file from S3: %s" (string-trim (cdr result)))))))

(defun s3-publish--remove-document-internal (file profile)
  "Remove FILE and its published .html companion from S3 using PROFILE.
Both objects share a key derived from FILE, differing only in extension.
The RSS feed and HTML index entries are dropped using the HTML URL, since
that is the URL those listings link to.  Returns the source S3 URI."
  (let* ((salt (plist-get profile :salt))
         (source-key (s3-publish-get-file-key file salt))
         (html-key (concat (file-name-sans-extension source-key) ".html"))
         (bucket (plist-get profile :bucket)))
    (s3-publish--delete-key profile source-key)
    (unless (string= source-key html-key)
      (unless (s3-publish--delete-key profile html-key t)
        (message "No published HTML companion found at %s" html-key)))
    (when (and (plist-get profile :rss-feed)
               (plist-get profile :public-acl))
      (s3-publish-remove-from-feed profile (s3-publish--object-url profile html-key)))
    (message "Successfully removed %s and %s from S3" source-key html-key)
    (format "s3://%s/%s" bucket source-key)))

(defun s3-publish-remove-document (file)
  "Interactively remove FILE and its published HTML companion from S3.
Use this for sources published with `s3-publish-org-buffer' or
`s3-publish-markdown-buffer', which upload two objects per document.
Returns the S3 URI of the removed source file."
  (interactive "fFile to remove from S3: ")
  (s3-publish--remove-document-internal file (s3-publish--select-profile)))


(defun s3-publish-buffer ()
  "Publish the entire current buffer to S3.
The contents of the current buffer are saved to a temporary file.
If the buffer is visiting a file, its extension is used; otherwise, \".txt\"
is used as the default extension. The file is then uploaded using an S3 profile
chosen interactively from `s3-publish-profiles'. The resulting URL is copied to
the kill ring and displayed in the echo area."
  (interactive)
  (let* ((buffer-filename (buffer-file-name))
         (ext (if (and buffer-filename (file-name-extension buffer-filename))
                  (concat "." (file-name-extension buffer-filename))
                ".txt"))
         (temp-file (make-temp-file "s3-publish-buffer-" nil ext))
         (contents (buffer-substring-no-properties (point-min) (point-max)))
         ;; Prepare title for RSS feed
         (title (or (and buffer-filename (file-name-nondirectory buffer-filename))
                    (buffer-name)))
         (description (format "Buffer content published on %s"
                             (format-time-string "%Y-%m-%d %H:%M"))))
    (with-temp-file temp-file
      (insert contents))
    ;; Prompt for a profile.
    (let* ((profile-name (s3-publish--read-profile-name))
           (profile
            (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
           (upload-result (s3-publish-upload-file temp-file profile title description))
           (url (if (stringp upload-result) upload-result upload-result)))
      (delete-file temp-file)
      (kill-new url)
      (message "%s" url)
      url)))

(defun s3-publish-region (start end)
  "Publish the currently selected region to S3.
The region from START to END is saved to a temporary .txt file,
then uploaded using an S3 profile chosen interactively from
`s3-publish-profiles'. The resulting URL is copied to the kill ring
and displayed in the echo area."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let* ((temp-file (make-temp-file "s3-publish-region-" nil ".txt"))
         (region-text (buffer-substring-no-properties start end))
         (buffer-filename (buffer-file-name))
         (title (format "Text from %s"
                       (or (and buffer-filename
                                (file-name-nondirectory buffer-filename))
                           (buffer-name))))
         (description (format "Text selection published on %s"
                            (format-time-string "%Y-%m-%d %H:%M"))))
    (with-temp-file temp-file
      (insert region-text))
    ;; Prompt for a profile.
    (let* ((profile-name (s3-publish--read-profile-name))
           (profile (s3-publish-get-credentials
                     (s3-publish-get-profile profile-name)))
           (upload-result (s3-publish-upload-file temp-file profile title description))
           (url (if (stringp upload-result) upload-result upload-result)))
      (delete-file temp-file)
      (kill-new url)
      (message "%s" url)
      url)))

;; Add command to view RSS feed or HTML index
(defun s3-publish-view-index (profile-name)
  "View the HTML index for the specified PROFILE-NAME.
If the index doesn't exist yet, offer to create it."
  (interactive
   (list (s3-publish--read-profile-name)))
  (let* ((profile (s3-publish-get-credentials
                   (s3-publish-get-profile profile-name)))
         (bucket (plist-get profile :bucket))
         (endpoint (plist-get profile :endpoint))
         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
         (host-base (replace-regexp-in-string "/$" "" host-base))
         (host-base (if (string-prefix-p (concat bucket ".") host-base)
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (concat bucket ".")))
                         "" host-base)
                      host-base))
         (index-url (format "https://%s.%s/%s"
                            bucket host-base s3-publish-html-index-filename)))

    ;; Check if RSS feed is enabled for this profile
    (unless (plist-get profile :rss-feed)
      (if (y-or-n-p "RSS feed not enabled for this profile. Enable it now? ")
          (let* ((edited-profile
                  (append profile
                          (list :rss-feed t
                                :rss-title (read-string "RSS feed title: "
                                                       (format "%s Files" profile-name))
                                :rss-desc (read-string "RSS feed description: "
                                                      "Files published from Emacs")
                                :rss-link (format "https://%s.%s" bucket host-base)))))
            (setq s3-publish-profiles
                  (mapcar (lambda (p)
                            (if (string= (plist-get p :name) profile-name)
                                edited-profile
                              p))
                          s3-publish-profiles))
            (s3-publish-save-profiles)
            (message "RSS feed enabled for profile '%s'" profile-name))
        (error "Cannot view index - RSS feed not enabled")))

    ;; Try to download the existing index
    (let ((html-index (s3-publish-download-html-index profile)))
      (if html-index
          ;; If index exists, open it in browser
          (browse-url index-url)
        ;; If index doesn't exist yet, offer to create it
        (when (y-or-n-p "HTML index doesn't exist yet. Create a new empty index? ")
          (s3-publish--put-string profile
                                  (s3-publish-create-new-html-index profile)
                                  s3-publish-html-index-filename "text/html")
          (message "Created new HTML index for %s" profile-name)
          (browse-url index-url))))))

(defun s3-publish-bucket-lifecycle (profile-name days-input)
  "Set or delete the S3 lifecycle expiration policy for the bucket PROFILE-NAME.
If DAYS-INPUT is non-empty, set the expiration policy using
  s3cmd expire --config CONFIG --expiry-days=DAYS s3://BUCKET
If DAYS-INPUT is empty, delete the lifecycle policy using
  s3cmd dellifecycle --config CONFIG s3://BUCKET
PROFILE-NAME is selected from `s3-publish-profiles' and used to retrieve
the bucket name and credentials. An error is signaled if no bucket is defined.
Prompts to use alternative credentials since this operation requires elevated privileges."
  (interactive (list (s3-publish--read-profile-name)
                     (read-string
                      "Enter number of expiry days (leave blank to delete policy): ")))
  (let* ((base-profile (s3-publish-get-profile profile-name))
         (profile
          (if (y-or-n-p "Lifecycle operations require admin privileges. Use alternative credentials? ")
              (s3-publish-select-credentials base-profile)
            (s3-publish-get-credentials base-profile)))
         (bucket (plist-get profile :bucket)))
    (unless bucket
      (error "No bucket defined for profile %s" profile-name))
    (if (string= days-input "")
        (progn
          (s3-publish--s3cmd-or-error profile "Deleting the lifecycle policy"
                                      "dellifecycle" (format "s3://%s" bucket))
          (message "Lifecycle policy deleted for bucket %s." bucket))
      (s3-publish--s3cmd-or-error profile "Setting the lifecycle policy"
                                  "expire"
                                  (format "--expiry-days=%s" days-input)
                                  (format "s3://%s" bucket))
      (message "Lifecycle policy set for bucket %s: objects expire after %s days"
               bucket days-input))))

(defun s3-publish-upload-multiple-files (files profile)
  "Upload multiple FILES to S3 using PROFILE.
FILES should be a list of file paths.
This function validates that every file exists and is not a directory
(allowing symlinks to files), and aborts if any file is invalid.
Each file is uploaded using `s3-publish-upload-file' with PROFILE.
All returned URLs (one per file) are concatenated (one per line),
copied to the kill ring, and returned."
  (unless (and files (cl-every (lambda (f)
                                 (and (file-exists-p f)
                                      (not (file-directory-p f))))
                               files))
    (error "One or more selected files do not exist or are not regular files"))
  (let ((urls (mapcar (lambda (file)
                        (s3-publish-upload-file file profile))
                      files))
        (result ""))
    (setq result (string-join urls "\n"))
    (kill-new result)
    (message "Uploaded files. URLs (one per line) copied to kill ring:\n%s"
             result)
    result))

(defun s3-publish-dired-upload-files ()
  "In Dired, upload the marked files to S3.
The function first retrieves the marked files, then prompts for an S3 profile
(from `s3-publish-profiles`). It then validates that all files exist and are
not directories before uploading them. The resulting URLs (one per line) are
copied to the kill ring and displayed."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (profile-name (s3-publish--read-profile-name))
         (profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (result (s3-publish-upload-multiple-files files profile)))
    result))

(defun s3-publish-remove-from-feed (profile file-url)
  "Remove an entry with FILE-URL from the RSS feed and HTML index for PROFILE.
Returns t if successful, nil if no modification was needed."
  (let ((any-updates nil))
    ;; Only proceed if RSS feed is enabled for this profile
    (when (plist-get profile :rss-feed)
      (let ((current-feed (s3-publish-download-feed profile))
            (current-index (s3-publish-download-html-index profile))
            new-feed
            new-index
            feed-modified
            index-modified)

        ;; Only proceed if we have existing feeds to modify
        (when (or current-feed current-index)
          (message "Processing removal of %s from feeds..." file-url)
          ;; Update the RSS feed
          (when current-feed
            (setq feed-modified nil)
            (setq new-feed
                  (with-temp-buffer
                    (insert current-feed)
                    (goto-char (point-min))
                    ;; Look for items with the matching URL
                    (let* ((case-fold-search t)
                           (url-pattern (regexp-quote (string-trim file-url)))
                           (found nil))
                      ;; Try multiple patterns to match the URL
                      (or
                       ;; Try exact match
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward (format "<link>%s</link>" url-pattern) nil t)
                           (message "Found exact URL match in feed")
                           (setq found t)))
                       ;; Try match with extra whitespace
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward (format "<link>[ \t\n]*%s[ \t\n]*</link>" url-pattern) nil t)
                           (message "Found URL with whitespace in feed")
                           (setq found t)))
                       ;; Try match with URL in CDATA
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward (format "<link><!\\[CDATA\\[%s\\]\\]></link>" url-pattern) nil t)
                           (message "Found URL in CDATA in feed")
                           (setq found t))))
                      ;; If found, remove the entire item
                      (when found
                        ;; Move to start of the item containing this URL
                        (let ((item-end (point))
                              item-start)
                          (if (re-search-backward "<item>" nil t)
                              (setq item-start (point))
                            (error "Found URL but couldn't locate start of item"))
                          ;; Find the end of the item
                          (goto-char item-end)
                          (unless (re-search-forward "</item>" nil t)
                            (error "Found URL but couldn't locate end of item"))
                          (setq item-end (point))

                          ;; Remove the entire item
                          (delete-region item-start item-end)
                          (setq feed-modified t)
                          (message "Removed item from RSS feed")
                          ;; Update the lastBuildDate
                          (goto-char (point-min))
                          (when (re-search-forward "<lastBuildDate>[^<]*</lastBuildDate>" nil t)
                            (replace-match (format "<lastBuildDate>%s</lastBuildDate>"
                                                   (s3-publish-format-rfc822-date (current-time))))))))
                    (buffer-string))))
          ;; Update the HTML index
          (when current-index
            (setq index-modified nil)
            (setq new-index
                  (with-temp-buffer
                    (insert current-index)
                    (goto-char (point-min))
                    ;; Look for entries with the matching URL
                    (let* ((case-fold-search t)
                           (url-pattern (regexp-quote (string-trim file-url)))
                           (found nil))
                      ;; Try multiple patterns to match the URL
                      (or
                       ;; Try exact match in href
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward (format "href=\"%s\"" url-pattern) nil t)
                           (message "Found exact URL match in HTML index")
                           (setq found t)))
                       ;; Try match with extra whitespace
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward (format "href=\"[ \t\n]*%s[ \t\n]*\"" url-pattern) nil t)
                           (message "Found URL with whitespace in HTML index")
                           (setq found t))))
                      ;; If found, remove the entire entry
                      (when found
                        ;; Move to start of the entry containing this URL
                        (let ((entry-end (point))
                              entry-start)
                          (if (re-search-backward "<div class=\"entry\">" nil t)
                              (setq entry-start (point))
                            (error "Found URL but couldn't locate start of entry"))
                          ;; Find the end of the entry
                          (goto-char entry-end)
                          (unless (re-search-forward "</div>[ \t\n]*</div>" nil t)
                            (error "Found URL but couldn't locate end of entry"))
                          (setq entry-end (point))
                          ;; Remove the entire entry
                          (delete-region entry-start entry-end)
                          (setq index-modified t)
                          (message "Removed entry from HTML index"))))
                    (buffer-string))))
          ;; If we modified either feed, upload the updates
          (if (or feed-modified index-modified)
              (progn
                (message "Feeds modified, uploading updates...")
                (when feed-modified
                  (s3-publish--put-string profile new-feed s3-publish-rss-filename
                                          "application/rss+xml")
                  (message "Updated RSS feed for %s - removed entry"
                           (plist-get profile :name))
                  (setq any-updates t))
                (when index-modified
                  (s3-publish--put-string profile new-index
                                          s3-publish-html-index-filename
                                          "text/html")
                  (message "Updated HTML index for %s - removed entry"
                           (plist-get profile :name))
                  (setq any-updates t)))
            ;; If we didn't modify anything, tell the user
            (message "URL %s not found in any feeds - nothing to update" file-url)))))
    ;; Return whether we updated anything
    any-updates))

(defun s3-publish--remove-file-internal (file profile)
  "Remove FILE from S3 using PROFILE without prompting.
PROFILE is a plist that must include :bucket, :endpoint, :salt,
and credentials (:access-key and :secret-key).
FILE's key is generated using `s3-publish-get-file-key' with PROFILE's
salt.  The RSS feed and HTML index entries are dropped too.
Returns the S3 URI if successful, or signals an error."
  (let* ((key (s3-publish-get-file-key file (plist-get profile :salt)))
         (s3-uri (format "s3://%s/%s" (plist-get profile :bucket) key)))
    (s3-publish--delete-key profile key)
    (when (and (plist-get profile :rss-feed)
               (plist-get profile :public-acl))
      (s3-publish-remove-from-feed profile (s3-publish--object-url profile key)))
    (message "Successfully removed %s from S3" s3-uri)
    s3-uri))

(defun s3-publish--url-key (profile url)
  "Return the object key that URL names in PROFILE's bucket.
URL is a public https://BUCKET.HOST/KEY URL or an s3://BUCKET/KEY URI.
Signals an error when URL does not belong to PROFILE's bucket."
  (let* ((url (string-trim url))
         (bucket (plist-get profile :bucket))
         (parsed (url-generic-parse-url url))
         (host (or (url-host parsed) ""))
         (path (car (url-path-and-query parsed)))
         (key (and path (> (length path) 1) (substring path 1))))
    (cond
     ((string= (url-type parsed) "s3")
      (unless (string= host bucket)
        (error "Bucket mismatch: URL bucket '%s' does not match profile bucket '%s'"
               host bucket)))
     ((not (string-match "\\`\\([^.]+\\)\\.\\(.+\\)\\'" host))
      (error "Cannot parse bucket from URL: %s" url))
     ((not (string= (match-string 1 host) bucket))
      (error "Bucket mismatch: URL bucket '%s' does not match profile bucket '%s'"
             (match-string 1 host) bucket))
     ((not (string= (match-string 2 host) (s3-publish--host-base profile)))
      (error "Host mismatch: URL host '%s' does not match profile endpoint '%s'"
             host (s3-publish--host-base profile))))
    (unless key
      (error "No S3 key found in URL: %s" url))
    key))

(defun s3-publish--remove-url (profile url)
  "Delete the object at URL from PROFILE's bucket, and its feed entry.
Returns the S3 URI of the deleted object."
  (let* ((key (s3-publish--url-key profile url))
         (s3-uri (format "s3://%s/%s" (plist-get profile :bucket) key)))
    (when (and (plist-get profile :rss-feed)
               (plist-get profile :public-acl))
      (when (s3-publish-remove-from-feed profile (string-trim url))
        (message "Updated feed to remove entry for %s" url)))
    (s3-publish--delete-key profile key)
    (message "Successfully removed %s" s3-uri)
    s3-uri))

(defun s3-publish-remove-urls (urls-string)
  "Remove multiple files from S3 given their public URLs.
URLS-STRING is a string with one URL per line.
Prompts for an S3 profile, then for each URL:
  - Parses the URL to extract the bucket and key.
  - Validates that the URL's bucket and host match the selected profile.
  - Removes the object.
  - Updates the RSS feed and HTML index if the profile has RSS enabled.
At the end, prints how many objects were deleted."
  (interactive "sS3 URLs to remove (one per line): ")
  (let* ((lines (split-string urls-string "\n" t "[ \t\n]+"))
         (profile-name (s3-publish--read-profile-name))
         (profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name))))
    (unless (and (plist-get profile :bucket) (plist-get profile :endpoint))
      (error "Profile '%s' must have both :bucket and :endpoint defined"
             profile-name))
    ;; Check every URL before deleting any, so one bad URL deletes nothing.
    (dolist (url lines)
      (s3-publish--url-key profile url))
    (dolist (url lines)
      (s3-publish--remove-url profile url))
    (message "Deleted %d object(s) from S3" (length lines))
    (length lines)))


(defun s3-publish-remove-file (file)
  "Interactively remove FILE from S3.
Prompts for an S3 profile (from `s3-publish-profiles') to use for removal.
Returns the S3 URI of the removed file if successful."
  (interactive "fFile to remove from S3: ")
  (let* ((profile-name (s3-publish--read-profile-name))
         (profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name))))
    (s3-publish--remove-file-internal file profile)))


(defun s3-publish-remove-buffer ()
  "Remove the file associated with the current buffer from S3.
This function calls `s3-publish-remove-file' on the current buffer's file path.
If the buffer is not visiting a file, it signals an error."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (s3-publish-remove-file file)
      (error "Current buffer is not visiting a file"))))



(defun s3-publish-dired-remove-files ()
  "In Dired, remove the marked files from S3.
The function retrieves the marked files, then prompts for an S3 profile
(from `s3-publish-profiles') once. It validates that all files exist and are
not directories. Each file is then removed using the internal removal function.
All resulting S3 URIs (one per file) are concatenated (one per line),
copied to the kill ring, and displayed."
  (interactive)
  (let* ((files (dired-get-marked-files)))
    ;; Validate that every file exists and is not a directory.
    (unless (cl-every (lambda (f)
                        (and (file-exists-p f)
                             (not (file-directory-p f))))
                      files)
      (error
       "One or more selected files do not exist or are not regular files"))
    (let* ((profile-name (s3-publish--read-profile-name))
           (profile
            (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
           (results (mapcar (lambda (f)
                              (s3-publish--remove-file-internal f profile))
                            files))
           (result-str (string-join results "\n")))
      (kill-new result-str)
      (message
       "Removed files. S3 URIs (one per line) copied to kill ring:\n%s"
       result-str)
      result-str)))


(defun s3-publish (&optional arg)
  "Publish to S3 in a context-aware (DWIM) way.
The dispatch priorities are:
1. If a region is active, run `s3-publish-region' on the selected region.
2. If the current buffer is in org-mode:
   - Without a universal argument, run `s3-publish-org-buffer'.
   - With a universal argument, run `s3-publish-remove-document' using the file
     backing the buffer, which removes both the .org source and its HTML.
3. If the current buffer holds Markdown (see `s3-publish--markdown-buffer-p'):
   - Without a universal argument, run `s3-publish-markdown-buffer'.
   - With a universal argument, run `s3-publish-remove-document' using the file
     backing the buffer, which removes both the .md source and its HTML.
4. If the current buffer is in dired-mode:
   - Without a universal argument, run `s3-publish-dired-upload-files'.
   - With a universal argument, run `s3-publish-dired-remove-files'.
5. Otherwise:
   - Without a universal argument, run `s3-publish-buffer'.
   - With a universal argument, run `s3-publish-remove-file' using the file backing the buffer.
ARG is the raw prefix argument."
  (interactive "P")
  (cond
   ;; Priority 1: Region is selected.
   ((use-region-p)
    (s3-publish-region (region-beginning) (region-end)))
   ;; Priority 2: Org-mode buffer.
   ((derived-mode-p 'org-mode)
    (if arg
        (if buffer-file-name
            (s3-publish-remove-document buffer-file-name)
          (error "Current org buffer is not visiting a file"))
      (s3-publish-org-buffer)))
   ;; Priority 3: Markdown buffer.
   ((s3-publish--markdown-buffer-p)
    (if arg
        (if buffer-file-name
            (s3-publish-remove-document buffer-file-name)
          (error "Current markdown buffer is not visiting a file"))
      (s3-publish-markdown-buffer)))
   ;; Priority 4: Dired buffer.
   ((derived-mode-p 'dired-mode)
    (if arg
        (s3-publish-dired-remove-files)
      (s3-publish-dired-upload-files)))
   ;; Priority 5: Any other buffer.
   (t
    (if arg
        (if buffer-file-name
            (s3-publish-remove-file buffer-file-name)
          (error "Current buffer is not visiting a file"))
      (s3-publish-buffer)))))
;; Modify the add-item-to-feed function to use the helper function
(defun s3-publish-add-item-to-feed (feed file-url title description pubdate)
  "Add a new item to an existing RSS FEED XML string, or update if URL exists.
FILE-URL is the URL of the published file.
TITLE is the title for the RSS item.
DESCRIPTION is the description for the RSS item.
PUBDATE is the publication date as a time value.
If the feed already contains an item with the same URL, the existing
item is updated with the new title, description, and date."
  (with-temp-buffer
    (insert feed)
    (goto-char (point-min))

    ;; Check if an item with this URL already exists
    (let ((match-data (s3-publish-feed-contains-url-p feed file-url)))
      (if match-data
          ;; If found, update the existing item
          (save-match-data
            (set-match-data match-data)
            (let ((item-start (match-beginning 0))
                  (item-end (match-end 0)))
              (delete-region item-start item-end)
              (goto-char item-start)
              (insert (format "    <item>
      <title>%s</title>
      <link>%s</link>
      <guid>%s</guid>
      <description><![CDATA[%s]]></description>
      <pubDate>%s</pubDate>
    </item>"
                             title
                             file-url
                             file-url
                             description
                             (s3-publish-format-rfc822-date pubdate)))))
        ;; If not found, add a new item before the closing </channel> tag
        (goto-char (point-min))
        (when (re-search-forward "</channel>" nil t)
          (goto-char (match-beginning 0))
          (insert (format "    <item>
      <title>%s</title>
      <link>%s</link>
      <guid>%s</guid>
      <description><![CDATA[%s]]></description>
      <pubDate>%s</pubDate>
    </item>\n    "
                          title
                          file-url
                          file-url
                          description
                          (s3-publish-format-rfc822-date pubdate)))))))

    ;; Update the lastBuildDate regardless of whether we added or updated
    (goto-char (point-min))
    (when (re-search-forward "<lastBuildDate>[^<]*</lastBuildDate>" nil t)
      (replace-match (format "<lastBuildDate>%s</lastBuildDate>"
                             (s3-publish-format-rfc822-date (current-time)))))
    (buffer-string))

;; Modify the add-item-to-html-index function to use the helper function
(defun s3-publish-add-item-to-html-index (html file-url title description pubdate)
  "Add a new item to an existing HTML index page, or update if URL exists.
HTML is the current HTML index content.
FILE-URL is the URL of the published file.
TITLE is the title for the index entry.
DESCRIPTION is the description for the index entry.
PUBDATE is the publication date as a time value.
If the index already contains an entry with the same URL, updates it instead."
  (with-temp-buffer
    (insert html)

    ;; Find the entries div and make sure it exists
    (goto-char (point-min))
    (unless (re-search-forward "<div class=\"entries\">" nil t)
      (error "Invalid HTML index format - no entries div found"))

    ;; Check if an entry with this URL already exists
    (let ((match-data (s3-publish-html-index-contains-url-p html file-url)))
      (if match-data
          ;; If found, update the existing entry
          (save-match-data
            (set-match-data match-data)
            (let ((entry-start (match-beginning 0))
                  (entry-end (match-end 0)))
              (delete-region entry-start entry-end)
              (goto-char entry-start)
              (insert (format "    <div class=\"entry\">
      <h2><a href=\"%s\">%s</a></h2>
      <div class=\"date\">%s</div>
      <div class=\"description\">%s</div>
      <div class=\"link\"><a href=\"%s\">%s</a></div>
    </div>"
                             file-url
                             title
                             (format-time-string "%Y-%m-%d %H:%M:%S" pubdate)
                             description
                             file-url
                             file-url))))
        ;; If not found, add a new entry at the top of the entries div
        (goto-char (point-min))
        (when (re-search-forward "<div class=\"entries\">" nil t)
          (goto-char (match-end 0))
          (insert (format "\n    <div class=\"entry\">
      <h2><a href=\"%s\">%s</a></h2>
      <div class=\"date\">%s</div>
      <div class=\"description\">%s</div>
      <div class=\"link\"><a href=\"%s\">%s</a></div>
    </div>"
                         file-url
                         title
                         (format-time-string "%Y-%m-%d %H:%M:%S" pubdate)
                         description
                         file-url
                         file-url)))))
    (buffer-string)))

;; Improve the feed search function to better handle XML
(defun s3-publish-feed-contains-url-p (feed url)
  "Check if FEED already contains an item with the given URL.
Returns the match data if found, nil otherwise."
  (with-temp-buffer
    (insert feed)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (url-pattern (regexp-quote url)))
      ;; We need a more precise regex that accounts for variations in XML
      (when (re-search-forward
             (format
              "<item>\\(\\(.\\|\n\\)*?\\)<link>\\(\\(.\\|\n\\)*?\\)%s\\(\\(.\\|\n\\)*?\\)</link>\\(\\(.\\|\n\\)*?\\)</item>" 
              url-pattern)
             nil t)
        (match-data)))))

;; Improve the HTML index search function
(defun s3-publish-html-index-contains-url-p (html url)
  "Check if HTML index already contains an entry with the given URL.
Returns the match data if found, nil otherwise."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (url-pattern (regexp-quote url)))
      ;; We need a more precise regex that accounts for variations in HTML
      (when (re-search-forward
             (format
              "<div class=\"entry\">\\(\\(.\\|\n\\)*?\\)<a href=\"\\(\\(.\\|\n\\)*?\\)%s\\(\\(.\\|\n\\)*?\\)\"\\(\\(.\\|\n\\)*?\\)</div>\\s-*</div>"
              url-pattern)
             nil t)
        (match-data)))))

;; Rewrite the update feed function to directly modify the files
(defun s3-publish-update-feed (profile file-url title description)
  "Update the RSS feed and HTML index for PROFILE with a new file.
FILE-URL is the URL of the newly published file.
TITLE is the title to use for the RSS item and HTML entry.
DESCRIPTION is the description for the RSS item and HTML entry.
If the feed or index already contains entries with the same URL,
they will be updated rather than duplicated."
  ;; Only proceed if RSS feed is enabled for this profile
  (when (plist-get profile :rss-feed)
    (let* ((current-feed (s3-publish-download-feed profile))
           (current-index (s3-publish-download-html-index profile))
           (pubdate (current-time))
           (new-feed-item (format "    <item>
      <title>%s</title>
      <link>%s</link>
      <guid>%s</guid>
      <description><![CDATA[%s]]></description>
      <pubDate>%s</pubDate>
    </item>"
                                 title
                                 file-url
                                 file-url
                                 description
                                 (s3-publish-format-rfc822-date pubdate)))
           (new-html-item (format "    <div class=\"entry\">
      <h2><a href=\"%s\">%s</a></h2>
      <div class=\"date\">%s</div>
      <div class=\"description\">%s</div>
      <div class=\"link\"><a href=\"%s\">%s</a></div>
    </div>"
                                 file-url
                                 title
                                 (format-time-string "%Y-%m-%d %H:%M:%S" pubdate)
                                 description
                                 file-url
                                 file-url)))

      ;; Process RSS feed
      (let ((new-feed
             (if (not current-feed)
                 ;; If no feed exists, create a new one
                 (let ((base-feed (s3-publish-create-new-rss-feed profile)))
                   (with-temp-buffer
                     (insert base-feed)
                     (goto-char (point-min))
                     (when (re-search-forward "</channel>" nil t)
                       (goto-char (match-beginning 0))
                       (insert "\n" new-feed-item "\n"))
                     (buffer-string)))
               ;; If feed exists, check for existing entry
               (with-temp-buffer
                 (insert current-feed)
                 (goto-char (point-min))
                 (let ((url-pattern (regexp-quote file-url))
                       (found nil))
                   ;; Look for any item with this URL
                   (goto-char (point-min))
                   (if (re-search-forward
                        (format "<item>\\(\\(.\\|\n\\)*?\\)<link>\\(\\(.\\|\n\\)*?\\)%s\\(\\(.\\|\n\\)*?\\)</link>\\(\\(.\\|\n\\)*?\\)</item>"
                                url-pattern)
                        nil t)
                       ;; If found, replace the item
                       (progn
                         (setq found t)
                         (replace-match new-feed-item t t))
                     ;; If not found, add it before closing channel tag
                     (goto-char (point-min))
                     (when (re-search-forward "</channel>" nil t)
                       (goto-char (match-beginning 0))
                       (insert "\n" new-feed-item "\n")))

                   ;; Always update the lastBuildDate
                   (goto-char (point-min))
                   (when (re-search-forward "<lastBuildDate>[^<]*</lastBuildDate>" nil t)
                     (replace-match (format "<lastBuildDate>%s</lastBuildDate>"
                                            (s3-publish-format-rfc822-date (current-time)))))
                   (buffer-string)))))

            ;; Process HTML index
            (new-index
             (if (not current-index)
                 ;; If no index exists, create a new one with the entry
                 (let ((base-index (s3-publish-create-new-html-index profile)))
                   (with-temp-buffer
                     (insert base-index)
                     (goto-char (point-min))
                     (when (re-search-forward "<div class=\"entries\">" nil t)
                       (goto-char (match-end 0))
                       (insert "\n" new-html-item))
                     (buffer-string)))
               ;; If index exists, check for existing entry
               (with-temp-buffer
                 (insert current-index)
                 (goto-char (point-min))
                 (let ((url-pattern (regexp-quote file-url))
                       (found nil))
                   ;; Look for any entry with this URL
                   (goto-char (point-min))
                   (if (re-search-forward
                        (format "<div class=\"entry\">\\(\\(.\\|\n\\)*?\\)<a href=\"\\(\\(.\\|\n\\)*?\\)%s\\(\\(.\\|\n\\)*?\\)\"\\(\\(.\\|\n\\)*?\\)</div>\\s-*</div>"
                                url-pattern)
                        nil t)
                       ;; If found, replace the entry
                       (progn
                         (setq found t)
                         (replace-match new-html-item t t))
                     ;; If not found, add it to the entries div
                     (goto-char (point-min))
                     (when (re-search-forward "<div class=\"entries\">" nil t)
                       (goto-char (match-end 0))
                       (insert "\n" new-html-item)))
                   (buffer-string))))))

        (s3-publish--put-string profile new-feed s3-publish-rss-filename
                                "application/rss+xml")
        (s3-publish--put-string profile new-index s3-publish-html-index-filename
                                "text/html")
        (message "Updated RSS feed and HTML index for %s"
                 (plist-get profile :name))))))

(defun s3-publish-s3fs-mount (profile-name mountpoint)
  "Mount the S3 bucket from PROFILE-NAME at MOUNTPOINT using s3fs.
Checks if s3fs is installed, then mounts the bucket. The bucket
will be mounted with the appropriate credentials from the profile.
This allows direct file system access to the bucket contents.
When the mount is successful, opens the mountpoint in a dired buffer."
  (interactive
   (list
    ;; Prompt for profile name
    (s3-publish--read-profile-name)
    ;; Prompt for mountpoint
    (read-directory-name "Mount at directory: ")))

  ;; Check if s3fs is installed
  (unless (executable-find "s3fs")
    (error "s3fs is not installed or not in PATH. Please install s3fs first"))

  ;; Ensure mountpoint exists
  (unless (file-directory-p mountpoint)
    (if (y-or-n-p (format "Directory %s does not exist. Create it? " mountpoint))
        (make-directory mountpoint t)
      (error "Mount aborted - mountpoint does not exist")))

  ;; Get profile with credentials
  (let* ((profile (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (bucket (plist-get profile :bucket))
         (access-key (plist-get profile :access-key))
         (secret-key (plist-get profile :secret-key))
         (endpoint (plist-get profile :endpoint))
         (url-host-base (replace-regexp-in-string "^https?://" "" endpoint))
         (url-host-base (replace-regexp-in-string "/$" "" url-host-base))
         (host-base (if (string-prefix-p (concat bucket ".") url-host-base)
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (concat bucket "."))) "" url-host-base)
                      url-host-base))
         ;; Create a temporary credentials file for s3fs
         (passwd-file (make-temp-file "s3fs-credentials-"))
         (passwd-content (format "%s:%s:%s" bucket access-key secret-key))
         (process-buffer (get-buffer-create "*s3fs-process*")))

    ;; Write credentials to temporary file with secure permissions
    (with-temp-file passwd-file
      (insert passwd-content))
    (set-file-modes passwd-file #o600) ;; secure the credentials file

    ;; Build s3fs command with correct arguments
    (let* ((mount-process-name "s3fs-mount-process")
           (region (car (split-string host-base "\\.")))
           (s3fs-args (list
                       bucket  ;; bucket name
                       (expand-file-name mountpoint)  ;; mountpoint
                       "-o" (format "passwd_file=%s" passwd-file)
                       "-o" (format "url=https://%s" host-base)
                       "-o" "use_path_request_style"
                       "-o" (format "endpoint=%s" region)
                       "-o" "dbglevel=info")))

      ;; Clear the process buffer
      (with-current-buffer process-buffer
        (erase-buffer))

      ;; Start s3fs as an asynchronous process
      (make-process
       :name mount-process-name
       :buffer process-buffer
       :command (cons "s3fs" s3fs-args)
       :sentinel
       (lambda (proc event)
         (when (string-match-p "finished" event)
           ;; Run our verification function
           (s3-publish--check-mount mountpoint passwd-file bucket process-buffer))))

      ;; Show the process buffer
      (display-buffer process-buffer)

      ;; Return a message immediately
      (message "Starting s3fs mount process... Check %s for progress" 
               (buffer-name process-buffer)))))

;; Define a function to check if the mount is successful
(defun s3-publish--check-mount (mountpoint passwd-file bucket process-buffer)
  "Check if the mountpoint is successfully mounted with s3fs.
If the mount is successful, opens the mountpoint in a dired buffer."
  (let ((mounted nil))
    ;; Wait a bit for the mount to complete
    (sleep-for 1)

    ;; Check if mountpoint is now an s3fs mount
    ;; Try multiple verification methods
    (with-temp-buffer
      ;; Method 1: Check mount command output
      (call-process "mount" nil t nil)
      (goto-char (point-min))
      (when (re-search-forward (format "s3fs.*%s" (regexp-quote mountpoint)) nil t)
        (setq mounted t))

      ;; Method 2: Check if the mountpoint has s3fs filesystem type
      (unless mounted
        (erase-buffer)
        (call-process "stat" nil t nil "-f" "-c" "%T" mountpoint)
        (goto-char (point-min))
        (when (or (looking-at "fuse\\.s3fs") 
                  (looking-at "fuse"))
          (setq mounted t)))

      ;; Method 3: Check if we can list the directory (this might be slow)
      (unless mounted
        (condition-case nil
            (let ((files (directory-files mountpoint)))
              (when files
                (setq mounted t)))
          (error nil))))

    ;; Handle the verification result
    (if mounted
        (progn
          (message "S3 bucket %s successfully mounted at %s" bucket mountpoint)
          ;; Clean up the credentials file
          (when (file-exists-p passwd-file)
            (delete-file passwd-file))
          ;; Open the mountpoint in dired
          (dired mountpoint))
      (message "Warning: Cannot verify if s3fs mount succeeded. The mount may still be working. Check %s for details."
               (buffer-name process-buffer)))))

;; Update the unmount function to close any dired buffers for the unmounted directory
(defun s3-publish-s3fs-unmount (mountpoint)
  "Unmount the S3 bucket mounted at MOUNTPOINT.
Uses the fusermount command to unmount an s3fs filesystem.
Closes any dired buffers visiting the mountpoint."
  (interactive
   (list (read-directory-name "Unmount directory: ")))

  ;; Check if fusermount is installed
  (unless (executable-find "fusermount")
    (error "fusermount is not installed or not in PATH"))

  ;; Ensure mountpoint exists
  (unless (file-directory-p mountpoint)
    (error "Directory %s does not exist" mountpoint))

  ;; Find and kill any dired buffers viewing this directory or subdirectories
  (let ((expanded-mountpoint (expand-file-name mountpoint)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'dired-mode)
                   (string-prefix-p expanded-mountpoint
                                    (expand-file-name default-directory)))
          (kill-buffer buffer)))))

  (let ((output-buffer (get-buffer-create "*s3fs-unmount-output*")))
    (with-current-buffer output-buffer
      (erase-buffer))

    ;; Run fusermount to unmount asynchronously
    (make-process
     :name "s3fs-unmount-process"
     :buffer output-buffer
     :command (list "fusermount" "-u" (expand-file-name mountpoint))
     :sentinel
     (lambda (proc event)
       (when (string-match-p "finished" event)
         (if (= 0 (process-exit-status proc))
             (message "Successfully unmounted %s" mountpoint)
           (with-current-buffer output-buffer
             (message "Failed to unmount: %s" (buffer-string)))))))

    (display-buffer output-buffer)
    (message "Unmounting %s..." mountpoint)))

(defun s3-publish-select-credentials (profile)
  "Interactively select credentials for operations on PROFILE.
Presents a list of available auth-source entries that might be suitable
for S3 operations and allows the user to select one.
Returns a new plist combining the original PROFILE with the selected credentials.
If user cancels, returns the original profile with regular credentials."
  (let* ((profile-name (plist-get profile :name))
         ;; Get all auth-source entries that might be S3 credentials
         (auth-entries (auth-source-search :require '(:user :secret) :max 100)))

    ;; Filter to likely S3-related entries and create selection list
    (let* ((s3-entries (cl-remove-if-not
                       (lambda (entry)
                         (let ((host (plist-get entry :host)))
                           (and host
                                (or (string-match-p "s3" host)
                                    (string-match-p "space" host)
                                    (string-match-p "aws" host)
                                    (string-match-p "amazon" host)
                                    (string-match-p "minio" host)
                                    (string-match-p "storage" host)))))
                       auth-entries))
           (options (mapcar (lambda (entry)
                              (cons (format "%s (user: %s)"
                                          (plist-get entry :host)
                                          (plist-get entry :user))
                                  entry))
                           s3-entries))
           ;; Add option for regular credentials
           (options (append options
                           (list (cons (format "Default credentials for profile '%s'" profile-name) nil))))
           ;; Let user select
           (selection (completing-read
                      "Select credentials for S3 operation: "
                      options nil t))
           (selected-entry (cdr (assoc selection options))))

      (if selected-entry
          ;; Return profile with selected credentials
          (let* ((access-key (plist-get selected-entry :user))
                 (secret (let ((sec (plist-get selected-entry :secret)))
                           (if (functionp sec)
                               (funcall sec)
                             sec))))
            (append profile (list :access-key access-key :secret-key secret)))

        ;; User selected default credentials
        (s3-publish-get-credentials profile)))))


(provide 's3-publish)
