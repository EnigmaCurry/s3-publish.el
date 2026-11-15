;;; s3p.el --- Batch driver for the s3-publish skill  -*- lexical-binding: t; -*-

;; Run by `s3p' as:
;;
;;   emacs --batch -Q -L REPO -l s3p.el -f s3p-main COMMAND ARGS...
;;
;; A fresh Emacs with none of the user's configuration: only the
;; s3-publish settings are read from their custom file (see
;; `s3p-settings-file'), and credentials come from auth-source as usual.
;; Prints one JSON value on stdout; progress goes to stderr.  Exits 0 on
;; success, 1 on failure with {"error": "..."} on stdout.

(require 'json)
(require 's3-publish)
(require 'htmlize nil t)                ; Coloured source blocks, if found.

(defvar s3p-settings-file nil
  "The file the user's s3-publish settings are read from.")

(defun s3p--custom-forms (file)
  "Return the top-level forms of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun s3p--load-settings (file)
  "Set every `s3-publish-' option FILE saves through `custom-set-variables'.
Nothing else in FILE is evaluated."
  (dolist (form (s3p--custom-forms file))
    (when (eq (car-safe form) 'custom-set-variables)
      (dolist (entry (cdr form))
        (let* ((entry (if (eq (car-safe entry) 'quote) (cadr entry) entry))
               (symbol (car-safe entry)))
          (when (and (symbolp symbol)
                     (string-prefix-p "s3-publish-" (symbol-name symbol)))
            (set symbol (eval (cadr entry) t)))))))
  ;; This process exits when the command is done, so upload in the
  ;; foreground; the caller wants to know how it went.
  (setq s3-publish-async nil))

(defun s3p--load-history (file)
  "Read the profile history savehist keeps in FILE, if it does."
  (when (and file (file-readable-p file))
    (dolist (form (ignore-errors (s3p--custom-forms file)))
      (when (and (eq (car-safe form) 'setq)
                 (eq (cadr form) 's3-publish-profile-history))
        (setq s3-publish-profile-history (ignore-errors (eval (nth 2 form) t)))))))

(defun s3p--jsonify (value)
  "Turn VALUE into what `json-encode' cannot misread.
A plist becomes an object, any other list an array, a symbol a string."
  (cond
   ((and (consp value) (keywordp (car value)))
    (let ((object (make-hash-table :test #'equal)))
      (while value
        (puthash (substring (symbol-name (pop value)) 1)
                 (s3p--jsonify (pop value))
                 object))
      object))
   ((consp value) (vconcat (mapcar #'s3p--jsonify value)))
   ((vectorp value) (vconcat (mapcar #'s3p--jsonify value)))
   ((memq value '(t nil)) value)
   ((symbolp value) (symbol-name value))
   (t value)))

(defun s3p--json (value)
  "Print VALUE as JSON on stdout."
  (let ((json-encoding-pretty-print t))
    (princ (json-encode (s3p--jsonify value)))
    (terpri)))

(defun s3p--usage ()
  (error "Usage: s3p profiles | publish FILE PROFILE [--title T] [--run-code] \
| list PROFILE [--bucket] | delete TARGET PROFILE | sign TARGET DURATION PROFILE"))

(defun s3p--run (command args)
  "Run COMMAND with ARGS, returning what to print."
  (pcase command
    ("check"
     (list :ok t :settings s3p-settings-file
           :library (locate-library "s3-publish")
           :htmlize (and (featurep 'htmlize) t)
           :s3cmd (executable-find "s3cmd")
           :profiles (length s3-publish-profiles)))
    ("profiles" (vconcat (s3-publish-api-profiles)))
    ("publish"
     (let ((file nil) (profile nil) (title nil) (run-code nil))
       (while args
         (let ((arg (pop args)))
           (pcase arg
             ("--title" (setq title (or (pop args) (s3p--usage))))
             ("--run-code" (setq run-code t))
             (_ (cond ((null file) (setq file arg))
                      ((null profile) (setq profile arg))
                      (t (s3p--usage)))))))
       (unless (and file profile) (s3p--usage))
       (let ((result (s3-publish-api-publish (expand-file-name file) profile
                                             title run-code)))
         (unless (eq (plist-get result :status) 'done)
           (error "%s" (or (plist-get result :error) "Upload did not complete")))
         (list :url (plist-get result :url) :keys (plist-get result :keys)
               :images (vconcat (plist-get result :images))))))
    ("list"
     (unless args (s3p--usage))
     (vconcat (s3-publish-api-list (car args) (equal (cadr args) "--bucket"))))
    ("delete"
     (unless (= (length args) 2) (s3p--usage))
     (list :deleted (vconcat (s3-publish-api-delete (car args) (cadr args)))))
    ("sign"
     (unless (= (length args) 3) (s3p--usage))
     (list :url (apply #'s3-publish-api-sign args)))
    (_ (s3p--usage))))

(defun s3p-main ()
  "Entry point: run the command in `command-line-args-left' and exit."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (condition-case err
        (progn
          (setq s3p-settings-file (getenv "S3P_SETTINGS_FILE"))
          (unless (and s3p-settings-file (file-readable-p s3p-settings-file))
            (error "No s3-publish settings file; set S3P_SETTINGS_FILE"))
          (s3p--load-settings s3p-settings-file)
          (s3p--load-history (getenv "S3P_HISTORY_FILE"))
          (unless s3-publish-profiles
            (error "No s3-publish-profiles in %s" s3p-settings-file))
          (s3p--json (s3p--run (car args) (cdr args)))
          (kill-emacs 0))
      (error
       (s3p--json (list :error (error-message-string err)))
       (kill-emacs 1)))))

;;; s3p.el ends here
