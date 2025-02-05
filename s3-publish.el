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

;; This enables publishing files (e.g., HTML) to S3 (compatible)
;; storage.

;;; Usage:
;;

(defgroup s3-publish nil
  "Settings for publishing files to S3-compatible storage."
  :prefix "s3-publish-"
  :group 'external)

(defcustom s3-publish-profiles nil
  "List of S3 publishing profiles.
Each profile is an alist containing the following keys:
  - :name       A unique identifier for the profile.
  - :endpoint   The URL of the S3-compatible endpoint.
  - :bucket     The bucket name to which files will be published.
  - :public-acl A boolean indicating if the published files should be publicly accessible.
The auth-source machine name will be derived by prepending \"s3-publish-\" to the profile name.
Example:
  '((:name \"default\"
     :endpoint \"https://s3.amazonaws.com\"
     :bucket \"my-bucket\"
     :public-acl t)
    (:name \"do-spaces\"
     :endpoint \"https://nyc3.digitaloceanspaces.com\"
     :bucket \"do-bucket\"
     :public-acl nil))
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
The auth-source machine is derived as \"s3-publish-<profile-name>\".
Returns a new plist combining the original PROFILE with keys :access-key and :secret-key.
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
            (insert (format "Name: %s\nEndpoint: %s\nBucket: %s\nPublic ACL: %s\n\n"
                            (plist-get p :name)
                            (plist-get p :endpoint)
                            (plist-get p :bucket)
                            (if (plist-get p :public-acl) "Yes" "No"))))
        (insert "No s3-publish profiles defined."))
      (goto-char (point-min))
      (read-only-mode 1)))
  (display-buffer s3-publish-profiles-buffer))

(defun s3-publish-manage-profiles ()
  "Manage s3-publish profiles interactively.
Immediately displays a list of profiles in a dedicated buffer.
Then, prompts you to Add, Edit, or Remove a profile.
The profiles buffer automatically updates after any change.
When you quit (with C-g), the profiles window is closed and the buffer is killed."
  (interactive)
  (s3-publish-refresh-profiles-buffer)
  (condition-case nil
      (while t
        (let ((action (completing-read "Manage s3-publish profiles (Add/Edit/Remove, C-g when done): "
                                       '("Add" "Edit" "Remove") nil t)))
          (cond
           ((string= action "Add")
            (s3-publish-add-profile))
           ((string= action "Edit")
            (s3-publish-edit-profile))
           ((string= action "Remove")
            (s3-publish-remove-profile)))
          (s3-publish-refresh-profiles-buffer)))
    (quit
     (let ((win (get-buffer-window s3-publish-profiles-buffer)))
       (when win
         (delete-window win)))
     (kill-buffer s3-publish-profiles-buffer)
     (message "Exiting s3-publish profile management."))))

(defun s3-publish-list-profiles ()
  "List all s3-publish profiles in a temporary buffer."
  (if s3-publish-profiles
      (with-output-to-temp-buffer "*s3-publish-profiles*"
        (dolist (p s3-publish-profiles)
          (princ (format "Name: %s\nEndpoint: %s\nBucket: %s\nPublic ACL: %s\n\n"
                         (plist-get p :name)
                         (plist-get p :endpoint)
                         (plist-get p :bucket)
                         (if (plist-get p :public-acl) "Yes" "No")))))
    (message "No s3-publish profiles defined.")))

(defun s3-publish-add-profile ()
  "Add a new s3-publish profile interactively."
  (interactive)
  (let* ((name (read-string "Profile name: "))
         (endpoint (read-string "Endpoint URL: "))
         (bucket (read-string "Bucket name: "))
         (public-acl (y-or-n-p "Make files public? "))
         (new-profile (list :name name
                            :endpoint endpoint
                            :bucket bucket
                            :public-acl public-acl)))
    (setq s3-publish-profiles (append s3-publish-profiles (list new-profile)))
    (message "Profile '%s' added." name)
    (s3-publish-refresh-profiles-buffer)))

(defun s3-publish-remove-profile ()
  "Remove an existing s3-publish profile interactively."
  (interactive)
  (let* ((names (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (name (completing-read "Remove profile: " names nil t)))
    (setq s3-publish-profiles
          (cl-remove-if (lambda (p)
                          (string= (plist-get p :name) name))
                        s3-publish-profiles))
    (message "Profile '%s' removed." name)
    (s3-publish-refresh-profiles-buffer)))

(defun s3-publish-edit-profile ()
  "Edit an existing s3-publish profile interactively."
  (interactive)
  (let* ((names (mapcar (lambda (p) (plist-get p :name)) s3-publish-profiles))
         (name (completing-read "Edit profile: " names nil t))
         (profile (seq-find (lambda (p)
                              (string= (plist-get p :name) name))
                            s3-publish-profiles)))
    (if profile
        (let* ((new-endpoint (read-string (format "Endpoint URL (current: %s): "
                                                  (plist-get profile :endpoint))
                                          nil nil (plist-get profile :endpoint)))
               (new-bucket (read-string (format "Bucket name (current: %s): "
                                                (plist-get profile :bucket))
                                         nil nil (plist-get profile :bucket)))
               (new-public-acl (y-or-n-p (format "Make files public? (current: %s): "
                                                 (if (plist-get profile :public-acl) "Yes" "No"))))
               (edited-profile (list :name name
                                     :endpoint new-endpoint
                                     :bucket new-bucket
                                     :public-acl new-public-acl)))
          (setq s3-publish-profiles
                (mapcar (lambda (p)
                          (if (string= (plist-get p :name) name)
                              edited-profile
                            p))
                        s3-publish-profiles))
          (message "Profile '%s' updated." name))
      (message "Profile '%s' not found." name))
    (s3-publish-refresh-profiles-buffer)))


;(s3-publish-get-credentials (s3-publish-get-profile "default"))

(provide 's3-publish)
