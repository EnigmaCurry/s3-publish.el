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

(defcustom s3-publish-profiles nil
  "List of S3 publishing profiles.
Each profile is an alist containing the following keys:
  - :name       A unique identifier for the profile.
  - :endpoint   The domain of the S3-compatible endpoint (minus bucket name).
  - :bucket     The bucket name to which files will be published.
  - :public-acl A boolean indicating if the file is public.
  - :salt       A random string used to salt file keys (generated when added).
The auth-source machine name will be derived by prepending \"s3-publish-\" to
the profile name.
Example:
  ((:name \"default\"
     :endpoint \"s3.amazonaws.com\"
     :bucket \"my-bucket\"
     :public-acl t
     :salt \"1a2b3c4d\")
    (:name \"do-spaces\"
     :endpoint \"nyc3.digitaloceanspaces.com\"
     :bucket \"do-bucket\"
     :public-acl nil
     :salt \"9e8f7d6c\"))
"
  :group 's3-publish
  :type '(repeat
          (alist :key-type symbol
                 :value-type (choice
                              (string :tag "String")
                              (boolean :tag "Boolean")))))

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

(defun s3-publish-refresh-profiles-buffer ()
  "Refresh the s3-publish profiles list in the dedicated buffer.
If the buffer does not exist, it is created."
  (with-current-buffer (get-buffer-create s3-publish-profiles-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if s3-publish-profiles
          (dolist (p s3-publish-profiles)
            (insert (format
              "Name: %s\nEndpoint: %s\nBucket: %s\nPublic ACL: %s\nSalt: %s\n\n"
                     (plist-get p :name)
                     (plist-get p :endpoint)
                     (plist-get p :bucket)
                     (if (plist-get p :public-acl) "Yes" "No")
                     (or (plist-get p :salt) "Not set")))
          (insert "No s3-publish profiles defined."))
      (goto-char (point-min))
      (read-only-mode 1)))
  (display-buffer s3-publish-profiles-buffer)))

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

(defun s3-publish-add-profile ()
  "Add a new s3-publish profile interactively.
Generates a random salt (an 8-character string) and stores it with the profile."
  (let* ((name (read-string "Profile name: "))
         (endpoint (read-string "Endpoint URL: "))
         (bucket (read-string "Bucket name: "))
         (public-acl (y-or-n-p "Make files public? "))
         (salt (substring (md5 (format "%s-%d" (current-time-string) (random)))
                          0 32))
         (new-profile (list :name name
                            :endpoint endpoint
                            :bucket bucket
                            :public-acl public-acl
                            :salt salt)))
    (setq s3-publish-profiles (append s3-publish-profiles (list new-profile)))
    (s3-publish-save-profiles)
    (message "Profile '%s' added." name)
    (s3-publish-refresh-profiles-buffer)))

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
               ;; Retain the existing salt.
               (edited-profile (list :name name
                                     :endpoint new-endpoint
                                     :bucket new-bucket
                                     :public-acl new-public-acl
                                     :salt (plist-get profile :salt))))
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

(defun s3-publish-upload-file (file profile)
  "Upload FILE to S3 using PROFILE.
PROFILE is a plist that must include :bucket, :endpoint, and credentials
(:access-key and :secret-key), and now also :salt.
The file’s key is generated using `s3-publish-get-file-key' with the profile’s
salt,
and a temporary s3cmd config file is created using
`s3-publish-generate-s3cmd-config'.
The s3cmd command is run:
  s3cmd --config TMP_CONFIG [--acl-public] put FILE s3://BUCKET/KEY
If the upload is successful and the profile’s :public-acl is non-nil,
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
              (if (plist-get profile :public-acl)
                  (format "https://%s.%s/%s" bucket host-base key)
                s3-uri)
            (with-current-buffer output-buffer
              (error "Upload failed: %s" (buffer-string)))))
      (kill-buffer output-buffer))))

(defun s3-publish-org-buffer ()
  "Export the current Org buffer to HTML and upload it to S3.
Before exporting, save the buffer and back up the original .org file to S3.
Both files share the same key (computed from the org file and profile salt)
with only their extensions differing.
A link to the .org file is appended to the bottom of the HTML file.
Only the HTML file URL is displayed and copied to the kill ring."
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
         (html-key (concat base-key ".html")))
    (cl-labels ((upload-with-key (file profile key)
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
                              (if (plist-get profile :public-acl)
                                  (format "https://%s.%s/%s" bucket host-base key)
                                s3-uri)
                            (with-current-buffer output-buffer
                              (error "Upload failed: %s" (buffer-string)))))
                      (kill-buffer output-buffer)))))
      ;; Backup the original .org file and capture its URL.
      (let ((org-url (upload-with-key org-file profile org-key)))
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
          ;; Upload the modified HTML file using html-key.
          (let ((html-url (upload-with-key html-file profile html-key)))
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
         (contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-file temp-file
      (insert contents))
    ;; Prompt for a profile.
    (let* ((profile-names (mapcar (lambda (p) (plist-get p :name))
                                  s3-publish-profiles))
           (profile-name
            (completing-read "Select S3 profile: " profile-names nil t))
           (profile
            (s3-publish-get-credentials (s3-publish-get-profile profile-name)))
           (upload-result (s3-publish-upload-file temp-file profile))
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
         (region-text (buffer-substring-no-properties start end)))
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
           (upload-result (s3-publish-upload-file temp-file profile))
           (url (if (stringp upload-result) upload-result upload-result)))
      (delete-file temp-file)
      (kill-new url)
      (message "%s" url)
      url)))

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

(defun s3-publish--remove-file-internal (file profile)
  "Remove FILE from S3 using PROFILE without prompting.
PROFILE is a plist that must include :bucket, :endpoint, :salt,
and credentials (:access-key and :secret-key).
This function generates FILE’s S3 key using
`s3-publish-get-file-key' with PROFILE’s salt,
creates a temporary s3cmd config file, and runs:
  s3cmd --config CONFIG del s3://BUCKET/KEY
Returns the S3 URI if successful,
or signals an error with the output from s3cmd."
  (let* ((salt (plist-get profile :salt))
         (key (s3-publish-get-file-key file salt))
         (bucket (plist-get profile :bucket))
         (s3-uri (format "s3://%s/%s" bucket key))
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
                (message "Successfully removed %s from S3" s3-uri)
                s3-uri)
            (with-current-buffer output-buffer
              (error "Error removing file from S3: %s" (buffer-string)))))
      (kill-buffer output-buffer))))

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

(defun s3-publish-remove-urls (urls-string)
  "Remove multiple files from S3 given their public URLs.
URLS-STRING is a string with one URL per line.
Prompts for an S3 profile, then for each URL:
  - Parses the URL to extract the bucket and key.
  - Validates that the URL’s bucket and host match the selected profile.
  - Constructs the S3 URI and invokes s3cmd to remove the object.
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
                              (message "Successfully removed %s" s3-uri)
                              t)
                          (with-current-buffer output-buffer
                            (error
                             "Error removing %s: %s" s3-uri (buffer-string)))))
                    (kill-buffer output-buffer)))))
            lines)))
      (message "Deleted %d object(s) from S3" (length results))
      (length results))))

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


(provide 's3-publish)
