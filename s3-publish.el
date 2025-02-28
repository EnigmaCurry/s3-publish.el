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
(require 'dired)
(require 'ox-html)

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
  (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
         (bucket (plist-get profile :bucket))
         (s3-uri (format "s3://%s/%s" bucket s3-publish-rss-filename))
         (temp-file (make-temp-file "s3-publish-feed-" nil ".xml"))
         (args (list "--config" s3cmd-config "get" s3-uri temp-file "--force"))
         (output-buffer (generate-new-buffer "*s3-publish-download-feed*"))
         (exit-code (apply 'call-process "s3cmd" nil output-buffer nil args))
         content)

    (unwind-protect
        (progn
          ;; Check both the exit code and if the file contains content
          (if (and (zerop exit-code)
                   (file-exists-p temp-file)
                   (> (file-attribute-size (file-attributes temp-file)) 0)
                   (not (with-current-buffer output-buffer
                          (goto-char (point-min))
                          (search-forward "WARNING: Not Found" nil t))))
              (progn
                (setq content (with-temp-buffer
                                (insert-file-contents temp-file)
                                (buffer-string)))
                (message "Downloaded existing RSS feed from %s" s3-uri)
                content)
            (message "No existing RSS feed found at %s, will create new one" s3-uri)
            nil))
      (kill-buffer output-buffer)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;; Similarly fix the HTML index download function
(defun s3-publish-download-html-index (profile)
  "Download the current HTML index for PROFILE from S3.
Returns the index content as a string if successful,
or nil if the index doesn't exist yet."
  (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
         (bucket (plist-get profile :bucket))
         (s3-uri (format "s3://%s/%s" bucket s3-publish-html-index-filename))
         (temp-file (make-temp-file "s3-publish-index-" nil ".html"))
         (args (list "--config" s3cmd-config "get" s3-uri temp-file "--force"))
         (output-buffer (generate-new-buffer "*s3-publish-download-index*"))
         (exit-code (apply 'call-process "s3cmd" nil output-buffer nil args))
         content)

    (unwind-protect
        (progn
          ;; Check both the exit code and if the file contains content
          (if (and (zerop exit-code)
                   (file-exists-p temp-file)
                   (> (file-attribute-size (file-attributes temp-file)) 0)
                   (not (with-current-buffer output-buffer
                          (goto-char (point-min))
                          (search-forward "WARNING: Not Found" nil t))))
              (progn
                (setq content (with-temp-buffer
                                (insert-file-contents temp-file)
                                (buffer-string)))
                (message "Downloaded existing HTML index from %s" s3-uri)
                content)
            (message "No existing HTML index found at %s, will create new one" s3-uri)
            nil))
      (kill-buffer output-buffer)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

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
The file's key is generated using `s3-publish-get-file-key' with the profile's salt,
and a temporary s3cmd config file is created using
`s3-publish-generate-s3cmd-config'.
Optional TITLE and DESCRIPTION are used for RSS feed entries if the profile has
RSS feed enabled. If not provided, they will be generated from the filename.
The s3cmd command is run:
  s3cmd --config TMP_CONFIG [--acl-public] put FILE s3://BUCKET/KEY
If the upload is successful and the profile's :public-acl is non-nil,
this function returns the public URL for the file (constructed as
  https://BUCKET.HOST_BASE/KEY).
If :public-acl is nil, it returns the S3 URI (e.g., s3://BUCKET/KEY).
If the upload fails, it signals an error with the error message."
  (let* ((salt (plist-get profile :salt))
         (key (s3-publish-get-file-key file salt))
         (s3cmd-config (s3-publish-generate-s3cmd-config profile))
         (bucket (plist-get profile :bucket))
         (endpoint (plist-get profile :endpoint))
         ;; Compute host-base: remove protocol and trailing slash.
         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
         (host-base (replace-regexp-in-string "/$" "" host-base))
         ;; Remove the bucket subdomain if it exactly matches.
         (host-base (if (string-prefix-p (concat bucket ".") host-base)
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (concat bucket ".")))
                         "" host-base)
                      host-base))
         (s3-uri (format "s3://%s/%s" bucket key))
         ;; Conditionally include --acl-public if :public-acl is non-nil.
         (acl-args (if (plist-get profile :public-acl)
                       (list "--acl-public")
                     nil))
         (args (append (list "--config" s3cmd-config) acl-args
                       (list "put" file s3-uri)
                       (list "--add-header=x-amz-tagging: temp=temp")))
         (output-buffer (generate-new-buffer "*s3-publish-upload-output*"))
         exit-code)
    (unwind-protect
        (progn
          (setq exit-code
                (apply 'call-process "s3cmd" nil output-buffer nil args))
          (if (zerop exit-code)
              (let ((url (if (plist-get profile :public-acl)
                            (format "https://%s.%s/%s" bucket host-base key)
                          s3-uri)))
                ;; Update RSS feed if enabled
                (when (and (plist-get profile :rss-feed)
                           (plist-get profile :public-acl))
                  (let ((item-title (or title (file-name-nondirectory file)))
                        (item-desc (or description
                                       (format "File published on %s"
                                              (format-time-string "%Y-%m-%d %H:%M")))))
                    (s3-publish-update-feed profile url item-title item-desc)))
                ;; Return the URL
                url)
            (with-current-buffer output-buffer
              (error "Upload failed: %s" (buffer-string)))))
      (kill-buffer output-buffer))))

;; Update the org buffer publication function to handle feed entries better
(defun s3-publish-org-buffer ()
  "Export the current Org buffer to HTML and upload it to S3.
Before exporting, save the buffer and back up the original .org file to S3.
Both files share the same key (computed from the org file and profile salt)
with only their extensions differing.
A link to the .org file is appended to the bottom of the HTML file.
Only the HTML file URL is displayed and copied to the kill ring.
When updating the RSS feed, both files are grouped as a single entry."
  (interactive)
  ;; Save the current buffer if modified.
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))
  (let* ((org-file (buffer-file-name))
         (profile-names (mapcar (lambda (p) (plist-get p :name))
                                s3-publish-profiles))
         (default (if (member "default" profile-names)
                      "default"
                    (car profile-names)))
         (profile-name (completing-read "Select S3 profile: " profile-names nil t nil nil default))
         (profile (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (salt (plist-get profile :salt))
         ;; Compute the full key using the org file and salt, then strip its extension.
         (full-key (s3-publish-get-file-key org-file salt))
         (base-key (file-name-sans-extension full-key))
         (org-key (concat base-key ".org"))
         (html-key (concat base-key ".html"))
         ;; Extract title and description from org file for RSS feed
         (org-title (or (save-excursion
                          (goto-char (point-min))
                          (and (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
                               (match-string 1)))
                        (file-name-nondirectory org-file)))
         (org-description (or (save-excursion
                                (goto-char (point-min))
                                (and (re-search-forward "^#\\+DESCRIPTION:\\s-*\\(.*\\)$" nil t)
                                     (match-string 1)))
                              (format "Published on %s" (format-time-string "%Y-%m-%d %H:%M")))))
    (cl-labels ((get-public-url (bucket key host-base)
                  (format "https://%s.%s/%s" bucket host-base key))
                (upload-file-no-feed (file profile key)
                  "Upload file without updating the RSS feed"
                  (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
                         (bucket (plist-get profile :bucket))
                         (endpoint (plist-get profile :endpoint))
                         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
                         (host-base (replace-regexp-in-string "/$" "" host-base))
                         (host-base (if (string-prefix-p (concat bucket ".") host-base)
                                        (replace-regexp-in-string (concat "^" (regexp-quote (concat bucket "."))) "" host-base)
                                      host-base))
                         (s3-uri (format "s3://%s/%s" bucket key))
                         (acl-args (if (plist-get profile :public-acl)
                                       (list "--acl-public")
                                     nil))
                         (args (append (list "--config" s3cmd-config) acl-args
                                       (list "put" file s3-uri)
                                       (list "--add-header=x-amz-tagging: temp=temp")))
                         (output-buffer (generate-new-buffer "*s3-publish-upload-output*"))
                         exit-code)
                    (unwind-protect
                        (progn
                          (setq exit-code (apply 'call-process "s3cmd" nil output-buffer nil args))
                          (if (zerop exit-code)
                              (let ((url (if (plist-get profile :public-acl)
                                             (get-public-url bucket key host-base)
                                           s3-uri)))
                                url)
                            (with-current-buffer output-buffer
                              (error "Upload failed: %s" (buffer-string)))))
                      (kill-buffer output-buffer)))))

      ;; Backup the original .org file (without updating the feed yet)
      (let ((org-url (upload-file-no-feed org-file profile org-key)))
        ;; Export the Org buffer to an HTML file.
        (let* ((html-file (org-html-export-to-html))
               (html-file (if (file-symlink-p html-file)
                              (file-truename html-file)
                            html-file)))
          ;; Insert a link to the .org file at the bottom of the HTML.
          (with-temp-buffer
            (insert-file-contents html-file)
            (goto-char (point-max))
            (if (search-backward "</body>" nil t)
                (progn
                  (goto-char (match-beginning 0))
                  (insert (format "<div id=\"org-download\"><a href=\"%s\">View Org file source</a></div>\n" org-url)))
              (progn
                (goto-char (point-max))
                (insert (format "\n<div id=\"org-download\"><a href=\"%s\">View Org file source</a></div>\n" org-url))))
            (write-region (point-min) (point-max) html-file nil 'quiet))

          ;; Upload the modified HTML file
          (let* ((html-url (upload-file-no-feed html-file profile html-key))
                 (bucket (plist-get profile :bucket))
                 (endpoint (plist-get profile :endpoint))
                 (host-base (replace-regexp-in-string "^https?://" "" endpoint))
                 (host-base (replace-regexp-in-string "/$" "" host-base))
                 (host-base (if (string-prefix-p (concat bucket ".") host-base)
                                (replace-regexp-in-string (concat "^" (regexp-quote (concat bucket "."))) "" host-base)
                              host-base)))

            ;; Now update RSS feed with a single entry containing links to both files
            (when (and (plist-get profile :rss-feed)
                       (plist-get profile :public-acl))
              ;; Create a more comprehensive description that includes both links
              (let* ((combined-description
                      (format "%s<p>View: <a href=\"%s\">HTML</a> | <a href=\"%s\">Org source</a></p>"
                              org-description html-url org-url)))
                ;; Update feed with the combined description, using HTML URL as the primary URL
                (s3-publish-update-feed profile html-url org-title combined-description)))

            ;; Return the HTML URL as this is the primary content
            (kill-new html-url)
            (message "%s" html-url)
            html-url))))))

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
    (let* ((profile-names (mapcar (lambda (p) (plist-get p :name))
                                  s3-publish-profiles))
           (profile-name
            (completing-read "Select S3 profile: " profile-names nil t))
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
    (let* ((profile-names
            (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
           (default
            (if (member "default" profile-names) "default" (car profile-names)))
           (profile-name
            (completing-read "Select S3 profile: "
                             profile-names nil t nil nil default))
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
   (list (completing-read "Select S3 profile: "
                          (mapcar (lambda (p) (plist-get p :name))
                                  s3-publish-profiles)
                          nil t)))
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
          ;; Create and upload a new empty index
          (let* ((new-index (s3-publish-create-new-html-index profile))
                 (index-temp-file (make-temp-file "s3-publish-index-" nil ".html")))
            (unwind-protect
                (progn
                  ;; Write index to temp file
                  (with-temp-file index-temp-file (insert new-index))

                  ;; Upload the index to S3
                  (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
                         (bucket (plist-get profile :bucket))
                         (index-s3-uri (format "s3://%s/%s" bucket s3-publish-html-index-filename))
                         (acl-args (if (plist-get profile :public-acl)
                                      (list "--acl-public")
                                    nil))
                         (index-args (append (list "--config" s3cmd-config)
                                            acl-args
                                            (list "put" index-temp-file index-s3-uri)
                                            (list "--mime-type=text/html")))
                         (output-buffer (generate-new-buffer "*s3-publish-upload-index*"))
                         index-exit-code)

                    (unwind-protect
                        (progn
                          ;; Upload the HTML index
                          (setq index-exit-code
                                (apply 'call-process "s3cmd" nil output-buffer nil index-args))
                          (if (zerop index-exit-code)
                              (progn
                                (message "Created new HTML index for %s" profile-name)
                                (browse-url index-url))
                            (with-current-buffer output-buffer
                              (error "Failed to upload HTML index: %s" (buffer-string)))))
                      (kill-buffer output-buffer))))

              ;; Clean up temp files
              (when (file-exists-p index-temp-file)
                (delete-file index-temp-file)))))))))

(defun s3-publish-bucket-lifecycle (profile-name days-input)
  "Set or delete the S3 lifecycle expiration policy for the bucket PROFILE-NAME.
If DAYS-INPUT is non-empty, set the expiration policy using
  s3cmd expire --config CONFIG --expiry-days=DAYS s3://BUCKET
If DAYS-INPUT is empty, delete the lifecycle policy using
  s3cmd dellifecycle --config CONFIG s3://BUCKET
PROFILE-NAME is selected from `s3-publish-profiles' and used to retrieve
the bucket name and credentials. An error is signaled if no bucket is defined."
  (interactive (list (completing-read "Select S3 profile: "
                                      (mapcar (lambda (p) (plist-get p :name))
                                              s3-publish-profiles)
                                      nil t)
               (read-string
               "Enter number of expiry days (leave blank to delete policy): ")))
  (let* ((profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (bucket (plist-get profile :bucket)))
    (unless bucket
      (error "No bucket defined for profile %s" profile-name))
    (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
           (command (if (string= days-input "")
                        "dellifecycle"
                      "expire"))
           (args (if (string= days-input "")
                     (list "--config" s3cmd-config (format "s3://%s" bucket))
                   (list "--config" s3cmd-config
                         (format "--expiry-days=%s" days-input)
                         (format "s3://%s" bucket))))
           (output-buffer (generate-new-buffer "*s3cmd-output*"))
           (exit-code (apply 'call-process "s3cmd" nil output-buffer nil
                             (cons command args))))
      (unwind-protect
          (if (zerop exit-code)
              (if (string= days-input "")
                  (message "Lifecycle policy deleted for bucket %s." bucket)
            (message
             "Lifecycle policy set for bucket %s: objects expire after %s days"
                         bucket days-input))
            (with-current-buffer output-buffer
              (error "Error updating lifecycle policy: %s" (buffer-string))))
        (kill-buffer output-buffer)))))

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
         (profile-names
          (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (profile-name
          (completing-read "Select S3 profile: " profile-names nil t))
         (profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (result (s3-publish-upload-multiple-files files profile)))
    result))

;; Function to remove entries from feeds
(defun s3-publish-remove-from-feed (profile file-url)
  "Remove an entry with FILE-URL from the RSS feed and HTML index for PROFILE.
Returns t if successful, nil if no modification was needed."
  ;; Only proceed if RSS feed is enabled for this profile
  (when (plist-get profile :rss-feed)
    (let ((current-feed (s3-publish-download-feed profile))
          (current-index (s3-publish-download-html-index profile))
          (modified nil))
      
      ;; Only proceed if we have existing feeds to modify
      (when (or current-feed current-index)
        ;; Update the RSS feed
        (when current-feed
          (with-temp-buffer
            (insert current-feed)
            (goto-char (point-min))
            ;; Look for items with the matching URL
            (let ((case-fold-search t)
                  (url-pattern (regexp-quote file-url)))
              (when (re-search-forward (format "<item>\\(.*?\\)<link>.*?%s.*?</link>\\(.*?\\)</item>" url-pattern) nil t)
                (replace-match "")
                (setq modified t)
                ;; Update the lastBuildDate
                (goto-char (point-min))
                (when (re-search-forward "<lastBuildDate>[^<]*</lastBuildDate>" nil t)
                  (replace-match (format "<lastBuildDate>%s</lastBuildDate>"
                                         (s3-publish-format-rfc822-date (current-time)))))))
            (when modified
              (setq current-feed (buffer-string)))))
        
        ;; Update the HTML index
        (when current-index
          (with-temp-buffer
            (insert current-index)
            (goto-char (point-min))
            ;; Look for entries with the matching URL
            (let ((case-fold-search t)
                  (url-pattern (regexp-quote file-url)))
              (when (re-search-forward (format "<div class=\"entry\">\\(.*?\\)<a href=\".*?%s.*?\"\\(.*?\\)</div>\\s-*</div>" url-pattern) nil t)
                (replace-match "")
                (setq modified t))))
            (when modified
              (setq current-index (buffer-string))))
        
        ;; If we modified either feed, upload the updates
        (when modified
          ;; Save the updated feeds to temp files
          (let ((feed-temp-file (when current-feed (make-temp-file "s3-publish-feed-" nil ".xml")))
                (index-temp-file (when current-index (make-temp-file "s3-publish-index-" nil ".html"))))
            (unwind-protect
                (progn
                  ;; Write feeds to temp files
                  (when feed-temp-file
                    (with-temp-file feed-temp-file (insert current-feed)))
                  (when index-temp-file
                    (with-temp-file index-temp-file (insert current-index)))
                  
                  ;; Upload the feeds to S3
                  (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
                         (bucket (plist-get profile :bucket))
                         (acl-args (if (plist-get profile :public-acl)
                                       (list "--acl-public")
                                     nil))
                         (output-buffer (generate-new-buffer "*s3-publish-upload-feed*")))
                    
                    (unwind-protect
                        (progn
                          ;; Upload the RSS feed if we modified it
                          (when feed-temp-file
                            (let* ((feed-s3-uri (format "s3://%s/%s" bucket s3-publish-rss-filename))
                                   (feed-args (append (list "--config" s3cmd-config)
                                                     acl-args
                                                     (list "put" feed-temp-file feed-s3-uri)
                                                     (list "--mime-type=application/rss+xml")))
                                   (feed-exit-code (apply 'call-process "s3cmd" nil output-buffer nil feed-args)))
                              (unless (zerop feed-exit-code)
                                (with-current-buffer output-buffer
                                  (error "Failed to upload updated RSS feed: %s" (buffer-string))))))
                          
                          ;; Upload the HTML index if we modified it
                          (when index-temp-file
                            (let* ((index-s3-uri (format "s3://%s/%s" bucket s3-publish-html-index-filename))
                                   (index-args (append (list "--config" s3cmd-config)
                                                      acl-args
                                                      (list "put" index-temp-file index-s3-uri)
                                                      (list "--mime-type=text/html")))
                                   (index-exit-code (apply 'call-process "s3cmd" nil output-buffer nil index-args)))
                              (unless (zerop index-exit-code)
                                (with-current-buffer output-buffer
                                  (error "Failed to upload updated HTML index: %s" (buffer-string))))))
                          
                          (message "Updated RSS feed and HTML index for %s - removed entry" (plist-get profile :name)))
                      (kill-buffer output-buffer))))
              
              ;; Clean up temp files
              (when (and feed-temp-file (file-exists-p feed-temp-file))
                (delete-file feed-temp-file))
              (when (and index-temp-file (file-exists-p index-temp-file))
                (delete-file index-temp-file))))
          t)))))

;; Update s3-publish--remove-file-internal to update the feed
(defun s3-publish--remove-file-internal (file profile)
  "Remove FILE from S3 using PROFILE without prompting.
PROFILE is a plist that must include :bucket, :endpoint, :salt,
and credentials (:access-key and :secret-key).
This function generates FILE's S3 key using
`s3-publish-get-file-key' with PROFILE's salt,
creates a temporary s3cmd config file, and runs:
  s3cmd --config CONFIG del s3://BUCKET/KEY
Returns the S3 URI if successful,
or signals an error with the output from s3cmd."
  (let* ((salt (plist-get profile :salt))
         (key (s3-publish-get-file-key file salt))
         (bucket (plist-get profile :bucket))
         (s3-uri (format "s3://%s/%s" bucket key))
         (endpoint (plist-get profile :endpoint))
         ;; Compute host-base: remove protocol and trailing slash.
         (host-base (replace-regexp-in-string "^https?://" "" endpoint))
         (host-base (replace-regexp-in-string "/$" "" host-base))
         ;; Remove the bucket subdomain if it exactly matches.
         (host-base (if (string-prefix-p (concat bucket ".") host-base)
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (concat bucket ".")))
                         "" host-base)
                      host-base))
         (file-url (if (plist-get profile :public-acl)
                       (format "https://%s.%s/%s" bucket host-base key)
                     s3-uri))
         (s3cmd-config (s3-publish-generate-s3cmd-config profile))
         (args (list "--config" s3cmd-config "del" s3-uri))
         (output-buffer (generate-new-buffer "*s3-publish-remove-output*"))
         exit-code)
    (unwind-protect
        (progn
          (setq exit-code
                (apply 'call-process "s3cmd" nil output-buffer nil args))
          (if (zerop exit-code)
              (progn
                ;; Update RSS feed if enabled
                (when (and (plist-get profile :rss-feed)
                           (plist-get profile :public-acl))
                  (s3-publish-remove-from-feed profile file-url))
                (message "Successfully removed %s from S3" s3-uri)
                s3-uri)
            (with-current-buffer output-buffer
              (error "Error removing file from S3: %s" (buffer-string)))))
      (kill-buffer output-buffer))))

(defun s3-publish-remove-urls (urls-string)
  "Remove multiple files from S3 given their public URLs.
URLS-STRING is a string with one URL per line.
Prompts for an S3 profile, then for each URL:
  - Parses the URL to extract the bucket and key.
  - Validates that the URL's bucket and host match the selected profile.
  - Constructs the S3 URI and invokes s3cmd to remove the object.
  - Updates the RSS feed and HTML index if the profile has RSS enabled.
At the end, prints how many objects were deleted."
  (interactive "sS3 URLs to remove (one per line): ")
  (let* ((lines (split-string urls-string "\n" t "[ \t\n]+"))
         (profile-names (mapcar (lambda (p) (plist-get p :name))
                                s3-publish-profiles))
         (profile-name
          (completing-read "Select S3 profile: " profile-names nil t))
         (profile
          (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
         (profile-bucket (plist-get profile :bucket))
         (endpoint (plist-get profile :endpoint)))
    (unless (and profile-bucket endpoint)
      (error "Profile '%s' must have both :bucket and :endpoint defined"
             profile-name))
    (let ((results
           (mapcar
            (lambda (url)
              (let* ((url (string-trim url))
                     (parsed (url-generic-parse-url url))
                     (url-host (url-host parsed))
                     (url-path (url-filename parsed))
                     ;; Expect host format: "BUCKET.host-base"
                     (host-parts (and url-host (split-string url-host "\\.")))
                     (url-bucket (if (and host-parts (car host-parts))
                                     (car host-parts)
                                (error "Cannot parse bucket from URL: %s" url)))
                     (url-host-base (if (cdr host-parts)
                                      (mapconcat 'identity (cdr host-parts) ".")
                             (error "Cannot parse host base from URL: %s" url)))
                     (key (if (> (length url-path) 1)
                              (substring url-path 1)
                            (error "No S3 key found in URL: %s" url)))
                     ;; Normalize the endpoint's host base.
                     (profile-host-base
                      (replace-regexp-in-string "^https?://" "" endpoint))
                     (profile-host-base
                      (replace-regexp-in-string "/$" "" profile-host-base))
                     (profile-host-base
                      (if (string-prefix-p
                           (concat profile-bucket ".") profile-host-base)
                          (replace-regexp-in-string
                           (concat "^"
                                   (regexp-quote (concat profile-bucket ".")))
                                                    "" profile-host-base)
                                          profile-host-base)))
                ;; Verify that the URL's bucket matches the profile's bucket.
                (unless (string= profile-bucket url-bucket)
    (error "Bucket mismatch: URL bucket '%s' does not match profile bucket '%s'"
                         url-bucket profile-bucket))
                ;; Verify that the URL's computed host base.
                (unless (string= profile-host-base url-host-base)
      (error "Host mismatch: URL host '%s' does not match profile endpoint '%s'"
                         url-host profile-host-base))
                ;; Construct the S3 URI and remove the file.
                (let* ((s3-uri (format "s3://%s/%s" profile-bucket key))
                       (s3cmd-config (s3-publish-generate-s3cmd-config profile))
                       (args (list "--config" s3cmd-config "del" s3-uri))
                       (output-buffer
                        (generate-new-buffer "*s3-publish-remove-urls-output*"))
                       exit-code)
                  (unwind-protect
                      (progn
                        (setq exit-code
                              (apply 'call-process "s3cmd"
                                     nil output-buffer nil args))
                        (if (zerop exit-code)
                            (progn
                              ;; Update RSS feed if enabled
                              (when (and (plist-get profile :rss-feed)
                                        (plist-get profile :public-acl))
                                (s3-publish-remove-from-feed profile url))
                              (message "Successfully removed %s" s3-uri)
                              t)
                          (with-current-buffer output-buffer
                            (error
                             "Error removing %s: %s" s3-uri (buffer-string)))))
                    (kill-buffer output-buffer)))))
            lines)))
      (message "Deleted %d object(s) from S3" (length results))
      (length results))))

(defun s3-publish-remove-file (file)
  "Interactively remove FILE from S3.
Prompts for an S3 profile (from `s3-publish-profiles') to use for removal.
Returns the S3 URI of the removed file if successful."
  (interactive "fFile to remove from S3: ")
  (let* ((profile-names
          (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (profile-name
          (completing-read "Select S3 profile: " profile-names nil t))
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
    (let* ((profile-names (mapcar (lambda (p) (plist-get p :name))
                                  s3-publish-profiles))
           (profile-name (completing-read
                            "Select S3 profile: " profile-names nil t))
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
   - With a universal argument, run `s3-publish-remove-file' using the file backing the buffer.
3. If the current buffer is in dired-mode:
   - Without a universal argument, run `s3-publish-dired-upload-files'.
   - With a universal argument, run `s3-publish-dired-remove-files'.
4. Otherwise:
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
            (s3-publish-remove-file buffer-file-name)
          (error "Current org buffer is not visiting a file"))
      (s3-publish-org-buffer)))
   ;; Priority 3: Dired buffer.
   ((derived-mode-p 'dired-mode)
    (if arg
        (s3-publish-dired-remove-files)
      (s3-publish-dired-upload-files)))
   ;; Priority 4: Any other buffer.
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

        ;; Save the updated feed and index to temp files
        (let ((feed-temp-file (make-temp-file "s3-publish-feed-" nil ".xml"))
              (index-temp-file (make-temp-file "s3-publish-index-" nil ".html")))
          (unwind-protect
              (progn
                ;; Write feed and index to temp files
                (with-temp-file feed-temp-file (insert new-feed))
                (with-temp-file index-temp-file (insert new-index))

                ;; Upload the feed to S3
                (let* ((s3cmd-config (s3-publish-generate-s3cmd-config profile))
                       (bucket (plist-get profile :bucket))
                       (feed-s3-uri (format "s3://%s/%s" bucket s3-publish-rss-filename))
                       (index-s3-uri (format "s3://%s/%s" bucket s3-publish-html-index-filename))
                       (acl-args (if (plist-get profile :public-acl)
                                     (list "--acl-public")
                                   nil))
                       (feed-args (append (list "--config" s3cmd-config)
                                          acl-args
                                          (list "put" feed-temp-file feed-s3-uri)
                                          (list "--mime-type=application/rss+xml")))
                       (index-args (append (list "--config" s3cmd-config)
                                           acl-args
                                           (list "put" index-temp-file index-s3-uri)
                                           (list "--mime-type=text/html")))
                       (output-buffer (generate-new-buffer "*s3-publish-upload-feed*"))
                       feed-exit-code
                       index-exit-code)

                  (unwind-protect
                      (progn
                        ;; Upload the RSS feed
                        (setq feed-exit-code
                              (apply 'call-process "s3cmd" nil output-buffer nil feed-args))
                        (unless (zerop feed-exit-code)
                          (with-current-buffer output-buffer
                            (error "Failed to upload RSS feed: %s" (buffer-string))))

                        ;; Upload the HTML index
                        (setq index-exit-code
                              (apply 'call-process "s3cmd" nil output-buffer nil index-args))
                        (unless (zerop index-exit-code)
                          (with-current-buffer output-buffer
                            (error "Failed to upload HTML index: %s" (buffer-string))))

                        (message "Updated RSS feed and HTML index for %s" (plist-get profile :name)))
                    (kill-buffer output-buffer))))

            ;; Clean up temp files
            (when (file-exists-p feed-temp-file)
              (delete-file feed-temp-file))
            (when (file-exists-p index-temp-file)
              (delete-file index-temp-file))))))))

(provide 's3-publish)
