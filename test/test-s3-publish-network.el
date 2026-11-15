;;; test-s3-publish-network.el --- s3-publish against a fake s3cmd  -*- lexical-binding: t; -*-

;; These tests run the real publishing commands, with `test/bin/s3cmd'
;; standing in for s3cmd.  It keeps objects in a temporary directory, so
;; what was uploaded, with which flags, can be checked afterwards.

(require 'ert)
(require 'cl-lib)
(require 's3-publish)

(defconst test-s3-publish--bin
  (expand-file-name "bin" (file-name-directory
                           (or load-file-name buffer-file-name)))
  "Directory holding the fake s3cmd.")

(defvar test-s3-publish--root nil
  "The fake bucket store of the test currently running.")

(defconst test-s3-publish--profile
  '(:name "test" :endpoint "fake-endpoint" :bucket "bkt" :public-acl t
          :salt "salt" :rss-feed t :rss-title "T" :rss-desc "D"
          :rss-link "https://bkt.fake-endpoint")
  "The profile the network tests publish with.")

(defmacro test-s3-publish-with-fake-s3 (&rest body)
  "Run BODY with s3cmd replaced by the fake and one profile, \"test\"."
  (declare (indent 0) (debug t))
  `(let* ((test-s3-publish--root (make-temp-file "fake-s3-" t))
          (exec-path (cons test-s3-publish--bin exec-path))
          (process-environment
           (append (list (concat "PATH=" test-s3-publish--bin ":"
                                 (getenv "PATH"))
                         (concat "FAKE_S3_ROOT=" test-s3-publish--root))
                   process-environment))
          (s3-publish-profiles (list test-s3-publish--profile))
          (s3-publish-async nil)
          (kill-ring nil))
     (cl-letf (((symbol-function 's3-publish--read-profile-name)
                (lambda () "test"))
               ((symbol-function 's3-publish-get-credentials)
                (lambda (profile)
                  (append profile (list :access-key "AK" :secret-key "SK")))))
       (unwind-protect (progn ,@body)
         (delete-directory test-s3-publish--root t)))))

(defun test-s3-publish--object (key)
  "Return the contents of KEY in the fake bucket, or nil."
  (let ((file (expand-file-name (concat "objects/bkt/" key)
                                test-s3-publish--root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun test-s3-publish--object-flags (key)
  "Return the s3cmd flags KEY was uploaded with, as a list of strings."
  (split-string (or (test-s3-publish--object (concat key ".args")) "") "\n" t))

(defun test-s3-publish--log ()
  "Return the fake s3cmd's call log as a list of lines."
  (let ((file (expand-file-name "log" test-s3-publish--root)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (split-string (buffer-string) "\n" t)))))

(defun test-s3-publish--key (file &optional ext)
  "Return the key FILE publishes to, with its extension replaced by EXT."
  (let ((key (s3-publish-get-file-key file "salt")))
    (if ext (concat (file-name-sans-extension key) ext) key)))

(defmacro test-s3-publish-with-file (var name contents &rest body)
  "Bind VAR to a file called NAME holding CONTENTS in a temp dir, run BODY."
  (declare (indent 3) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "s3-publish-src-" t))
            (,var (expand-file-name ,name ,dir)))
       (unwind-protect
           (progn
             (with-temp-file ,var (insert ,contents))
             ,@body)
         (delete-directory ,dir t)))))


;;; s3cmd plumbing

(ert-deftest s3-publish-test-net-buffer-upload ()
  "Publishing a buffer uploads it, lists it, and returns its URL."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "notes.txt" "hello"
      (with-current-buffer (find-file-noselect file)
        (unwind-protect
            (let ((url (s3-publish-buffer)))
              (should (string-prefix-p "https://bkt.fake-endpoint/" url))
              (should (equal (car kill-ring) url))
              (should (equal (test-s3-publish--object
                              (file-name-nondirectory url))
                             "hello"))
              (should (string-match-p (regexp-quote url)
                                      (test-s3-publish--object "index.xml")))
              (should (string-match-p (regexp-quote url)
                                      (test-s3-publish--object "index.html"))))
          (kill-buffer))))))

(ert-deftest s3-publish-test-net-config-is-deleted ()
  "The s3cmd config, which holds the secret key, is gone after each call."
  (test-s3-publish-with-fake-s3
    (let ((before (directory-files temporary-file-directory nil
                                   "\\`s3cmd-config-")))
      (should (zerop (car (s3-publish--s3cmd
                           (s3-publish-get-credentials test-s3-publish--profile)
                           "del" "s3://bkt/x"))))
      ;; The config existed while s3cmd ran...
      (should (string-prefix-p "config=present" (car (test-s3-publish--log))))
      ;; ...and not afterwards.
      (should (equal before (directory-files temporary-file-directory nil
                                             "\\`s3cmd-config-"))))))

(ert-deftest s3-publish-test-net-cache-control-header ()
  "Uploads carry `s3-publish-cache-control', and none when it is nil."
  (test-s3-publish-with-fake-s3
    (let ((profile (s3-publish-get-credentials test-s3-publish--profile)))
      (test-s3-publish-with-file file "a.txt" "a"
        (s3-publish--put profile file "a.txt")
        (should (member "--add-header=Cache-Control: no-cache"
                        (test-s3-publish--object-flags "a.txt")))
        (let ((s3-publish-cache-control "max-age=60"))
          (s3-publish--put profile file "b.txt"))
        (should (member "--add-header=Cache-Control: max-age=60"
                        (test-s3-publish--object-flags "b.txt")))
        (let ((s3-publish-cache-control nil))
          (s3-publish--put profile file "c.txt"))
        (should-not (seq-find (lambda (f) (string-match-p "Cache-Control" f))
                              (test-s3-publish--object-flags "c.txt")))))))

(ert-deftest s3-publish-test-net-failure-is-an-error ()
  "A failing s3cmd signals an error carrying its output."
  (test-s3-publish-with-fake-s3
    (let ((process-environment (cons "FAKE_S3_FAIL=1" process-environment)))
      (test-s3-publish-with-file file "a.txt" "a"
        (let ((err (should-error
                    (s3-publish--put
                     (s3-publish-get-credentials test-s3-publish--profile)
                     file "a.txt"))))
          (should (string-match-p "simulated failure" (cadr err))))))))

(ert-deftest s3-publish-test-net-get-missing-is-nil ()
  "Fetching a missing object returns nil rather than signalling."
  (test-s3-publish-with-fake-s3
    (should-not (s3-publish--get
                 (s3-publish-get-credentials test-s3-publish--profile)
                 "nope.txt"))))

(ert-deftest s3-publish-test-net-remove-url ()
  "Removing by URL deletes the object and drops it from the listings."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "a.txt" "a"
      (let* ((profile (s3-publish-get-credentials test-s3-publish--profile))
             (url (s3-publish-upload-file file profile "A" "desc")))
        (should (test-s3-publish--object (test-s3-publish--key file)))
        (s3-publish-remove-urls url)
        (should-not (test-s3-publish--object (test-s3-publish--key file)))
        (should-not (string-match-p (regexp-quote url)
                                    (test-s3-publish--object "index.xml")))))))

(ert-deftest s3-publish-test-net-remove-url-rejects-other-buckets ()
  "A URL from another bucket is refused before anything is deleted."
  (test-s3-publish-with-fake-s3
    (should-error (s3-publish-remove-urls
                   "https://other.fake-endpoint/a.txt"))
    (should-not (test-s3-publish--log))))

(ert-deftest s3-publish-test-net-url-key ()
  "Keys are recovered from both public URLs and s3:// URIs."
  (let ((profile test-s3-publish--profile))
    (should (equal (s3-publish--url-key profile "https://bkt.fake-endpoint/a/b.txt")
                   "a/b.txt"))
    (should (equal (s3-publish--url-key profile "s3://bkt/k.html") "k.html"))
    (should-error (s3-publish--url-key profile "s3://other/k.html"))
    (should-error (s3-publish--url-key profile "https://bkt.elsewhere/k"))))

(ert-deftest s3-publish-test-net-org-document ()
  "An Org buffer publishes its source and HTML, listed once in the feed."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n\nHello.\n"
      (with-current-buffer (find-file-noselect file)
        (unwind-protect
            (let ((url (s3-publish-org-buffer)))
              (should (equal url (concat "https://bkt.fake-endpoint/"
                                         (test-s3-publish--key file ".html"))))
              (should (string-match-p "Hello"
                                      (test-s3-publish--object
                                       (test-s3-publish--key file ".html"))))
              (should (equal (test-s3-publish--object (test-s3-publish--key file))
                             "#+TITLE: Doc\n\nHello.\n"))
              (should (string-match-p "<title>Doc</title>"
                                      (test-s3-publish--object "index.xml"))))
          (kill-buffer))))))


;;; Local images

(defun test-s3-publish--write-png (file)
  "Write a small PNG to FILE (the default favicon) and return its bytes."
  (let ((bytes (base64-decode-string
                (substring s3-publish-html-default-favicon
                           (length "data:image/png;base64,")))))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'binary))
      (write-region bytes nil file nil 'quiet))
    bytes))

(defun test-s3-publish--publish-markdown (file)
  "Publish the Markdown FILE and return its published HTML."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (progn
          (s3-publish-markdown-buffer)
          (test-s3-publish--object (test-s3-publish--key file ".html")))
      (kill-buffer))))

(ert-deftest s3-publish-test-net-markdown-image-is-inlined ()
  "A Markdown image beside the document is embedded in the page."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Doc\n\n![shot](shot.png)\n"
      (let ((png (expand-file-name "shot.png" (file-name-directory md))))
        (test-s3-publish--write-png png)
        (let ((html (test-s3-publish--publish-markdown md)))
          (should (string-match-p "<img src=\"data:image/png;base64," html))
          (should-not (string-match-p "src=\"shot.png\"" html))
          (should-not (test-s3-publish--object (test-s3-publish--key png))))))))

(ert-deftest s3-publish-test-net-image-upload-mode ()
  "With `upload', the image is uploaded beside the page and linked."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Doc\n\n![shot](img/shot.png)\n"
      (let* ((png (expand-file-name "img/shot.png" (file-name-directory md)))
             (bytes (test-s3-publish--write-png png))
             (s3-publish-html-local-images 'upload)
             (html (test-s3-publish--publish-markdown md))
             (key (test-s3-publish--key png)))
        (should (string-match-p
                 (regexp-quote (format "src=\"https://bkt.fake-endpoint/%s\"" key))
                 html))
        (should (equal (string-to-unibyte
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally
                           (expand-file-name (concat "objects/bkt/" key)
                                             test-s3-publish--root))
                          (buffer-string)))
                       bytes))
        ;; Images are content, so they carry the lifecycle tag too.
        (should (member "--add-header=x-amz-tagging: temp=temp"
                        (test-s3-publish--object-flags key)))))))

(ert-deftest s3-publish-test-net-large-image-is-uploaded ()
  "An image over the inline size limit is uploaded instead of embedded."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Doc\n\n![shot](shot.png)\n"
      (let* ((png (expand-file-name "shot.png" (file-name-directory md)))
             (s3-publish-html-inline-image-max-size 10))
        (test-s3-publish--write-png png)
        (should (string-match-p
                 (regexp-quote (test-s3-publish--key png))
                 (test-s3-publish--publish-markdown md)))
        (should (test-s3-publish--object (test-s3-publish--key png)))))))

(ert-deftest s3-publish-test-net-org-image-is-inlined ()
  "An Org file link to an image is embedded the same way."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file org "doc.org" "#+TITLE: Doc\n\n[[file:pics/a.png]]\n"
      (test-s3-publish--write-png
       (expand-file-name "pics/a.png" (file-name-directory org)))
      (with-current-buffer (find-file-noselect org)
        (unwind-protect
            (progn
              (s3-publish-org-buffer)
              (should (string-match-p
                       "<img src=\"data:image/png;base64,"
                       (test-s3-publish--object
                        (test-s3-publish--key org ".html")))))
          (kill-buffer))))))

(ert-deftest s3-publish-test-net-missing-and-remote-images-left-alone ()
  "Unreadable local images and images on the web are not rewritten."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md"
        "# Doc\n\n![gone](gone.png)\n\n![web](https://example.com/a.png)\n"
      (let ((html (test-s3-publish--publish-markdown md)))
        (should (string-match-p "src=\"gone.png\"" html))
        (should (string-match-p "src=\"https://example.com/a.png\"" html))))))

(ert-deftest s3-publish-test-net-local-images-can-be-left-alone ()
  "With `s3-publish-html-local-images' nil, image links are untouched."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Doc\n\n![shot](shot.png)\n"
      (test-s3-publish--write-png
       (expand-file-name "shot.png" (file-name-directory md)))
      (let ((s3-publish-html-local-images nil))
        (should (string-match-p "src=\"shot.png\""
                                (test-s3-publish--publish-markdown md)))))))

(ert-deftest s3-publish-test-local-image-file ()
  "Image sources resolve to local files, and URLs do not."
  (should (equal (s3-publish--local-image-file "a b.png" "/d/") "/d/a b.png"))
  (should (equal (s3-publish--local-image-file "img/a%20b.png" "/d/")
                 "/d/img/a b.png"))
  (should (equal (s3-publish--local-image-file "file:///x/a.png" "/d/") "/x/a.png"))
  (should (equal (s3-publish--local-image-file "file:a.png" "/d/") "/d/a.png"))
  (should (equal (s3-publish--local-image-file "a&amp;b.png" "/d/") "/d/a&b.png"))
  (should-not (s3-publish--local-image-file "https://e.com/a.png" "/d/"))
  (should-not (s3-publish--local-image-file "data:image/png;base64,AA" "/d/"))
  (should-not (s3-publish--local-image-file "//e.com/a.png" "/d/")))

;;; Background uploads

(defmacro test-s3-publish-async (&rest body)
  "Run BODY with background uploads on, and a clean queue and log."
  (declare (indent 0) (debug t))
  `(let ((s3-publish-async t)
         (s3-publish--queue nil)
         (s3-publish--running nil))
     (when (get-buffer "*s3-publish-log*")
       (kill-buffer "*s3-publish-log*"))
     (unwind-protect (progn ,@body)
       (s3-publish--wait 30))))

(defun test-s3-publish--log-buffer ()
  "Return the contents of *s3-publish-log*."
  (with-current-buffer (get-buffer-create "*s3-publish-log*")
    (buffer-string)))

(ert-deftest s3-publish-test-async-org-document ()
  "A background publish returns the URL at once and uploads in a child."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n\nHello.\n"
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (let ((url (s3-publish-org-buffer)))
                (should (equal (car kill-ring) url))
                (should (s3-publish--wait 30))
                (should (string-match-p
                         "Hello" (test-s3-publish--object
                                  (test-s3-publish--key file ".html"))))
                (should (string-match-p (regexp-quote url)
                                        (test-s3-publish--object "index.xml")))
                (should (string-match-p (concat "done " (regexp-quote url))
                                        (test-s3-publish--log-buffer)))
                ;; The child ran s3cmd with the config, and deleted it.
                (should (cl-every (lambda (l) (string-prefix-p "config=present" l))
                                  (test-s3-publish--log))))
            (kill-buffer)))))))

(ert-deftest s3-publish-test-async-buffer-cleans-up ()
  "A background buffer publish deletes its temporary copy afterwards."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (with-temp-buffer
        (insert "some text")
        (let* ((temps (lambda () (directory-files temporary-file-directory nil
                                                  "\\`s3-publish-\\(buffer\\|job\\)-")))
               (before (funcall temps))
               (url (s3-publish-buffer)))
          (should (s3-publish--wait 30))
          (should (equal (test-s3-publish--object (file-name-nondirectory url))
                         "some text"))
          (should (equal before (funcall temps))))))))

(ert-deftest s3-publish-test-async-failure-is-reported ()
  "A failed background upload is logged and warned about."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (let ((process-environment (cons "FAKE_S3_FAIL=1" process-environment))
            (warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _) (push message warnings))))
          (with-temp-buffer
            (insert "x")
            (let ((url (s3-publish-buffer)))
              (should (s3-publish--wait 30))
              (should (string-match-p (concat "FAILED " (regexp-quote url))
                                      (test-s3-publish--log-buffer)))
              (should (string-match-p "simulated failure" (car warnings))))))))))

(ert-deftest s3-publish-test-async-jobs-run-in-order-and-coalesce ()
  "Jobs run one at a time; a waiting job is replaced by a newer one."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (let ((profile (s3-publish-get-credentials test-s3-publish--profile)))
        (s3-publish--run "a" "a" 's3-publish--put-string
                         (list profile "first" "a.txt" "text/plain"))
        (s3-publish--run "b" "b" 's3-publish--put-string
                         (list profile "old" "b.txt" "text/plain"))
        (s3-publish--run "b" "b" 's3-publish--put-string
                         (list profile "new" "b.txt" "text/plain"))
        ;; "a" is running; only the newer "b" waits.
        (should s3-publish--running)
        (should (= 1 (length s3-publish--queue)))
        (should (s3-publish--wait 30))
        (should (equal (test-s3-publish--object "a.txt") "first"))
        (should (equal (test-s3-publish--object "b.txt") "new"))
        (should (= 2 (length (test-s3-publish--log))))))))

;;; Republishing on save

(defmacro test-s3-publish-with-on-save (var name contents &rest body)
  "Visit a file NAME holding CONTENTS with `s3-publish-on-save-mode' on.
VAR is bound to the file.  After the mode is on, asking for a profile is
an error, since saving must not prompt."
  (declare (indent 3) (debug t))
  `(test-s3-publish-with-file ,var ,name ,contents
     (with-current-buffer (find-file-noselect ,var)
       (unwind-protect
           (progn
             (s3-publish-on-save-mode 1)
             (should (equal s3-publish-on-save-profile "test"))
             (cl-letf (((symbol-function 's3-publish--read-profile-name)
                        (lambda () (error "Prompted for a profile"))))
               ,@body))
         (set-buffer-modified-p nil)
         (kill-buffer)))))

(defun test-s3-publish--edit-and-save (text)
  "Replace the current buffer's text with TEXT and save it."
  (erase-buffer)
  (insert text)
  (let ((inhibit-message t)
        (require-final-newline nil))
    (save-buffer)))

(ert-deftest s3-publish-test-on-save-org ()
  "Saving an Org buffer in the mode republishes it, without asking."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-on-save file "doc.org" "#+TITLE: Doc\n"
      (should-not (test-s3-publish--object "index.xml"))
      (test-s3-publish--edit-and-save "#+TITLE: Doc\n\nFirst.\n")
      (should (string-match-p "First" (test-s3-publish--object
                                        (test-s3-publish--key file ".html"))))
      (test-s3-publish--edit-and-save "#+TITLE: Doc\n\nSecond.\n")
      (should (string-match-p "Second" (test-s3-publish--object
                                         (test-s3-publish--key file ".html")))))))

(ert-deftest s3-publish-test-on-save-plain-file-keeps-its-url ()
  "A plain file is published as itself, so every save has the same URL."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-on-save file "notes.txt" "one"
      (test-s3-publish--edit-and-save "two")
      (should (equal (test-s3-publish--object (test-s3-publish--key file)) "two"))
      (test-s3-publish--edit-and-save "three")
      (should (equal (test-s3-publish--object (test-s3-publish--key file)) "three"))
      (should (equal (car kill-ring)
                     (concat "https://bkt.fake-endpoint/"
                             (test-s3-publish--key file)))))))

(ert-deftest s3-publish-test-on-save-failure-does-not-fail-the-save ()
  "A failed upload is reported, and the file is still saved."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-on-save file "notes.txt" "one"
      (let ((process-environment (cons "FAKE_S3_FAIL=1" process-environment)))
        (test-s3-publish--edit-and-save "two")
        (should-not (buffer-modified-p))
        (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                       "two"))))))

(ert-deftest s3-publish-test-on-save-needs-a-file ()
  "The mode refuses a buffer that is not visiting a file."
  (test-s3-publish-with-fake-s3
    (with-temp-buffer
      (should-error (s3-publish-on-save-mode 1) :type 'user-error)
      (should-not s3-publish-on-save-mode))))

(ert-deftest s3-publish-test-on-save-profile-from-local-variable ()
  "A profile named in the file's local variables is used without asking."
  (test-s3-publish-with-fake-s3
    (should (safe-local-variable-p 's3-publish-on-save-profile "test"))
    (test-s3-publish-with-file file "notes.txt" "x"
      (with-current-buffer (find-file-noselect file)
        (unwind-protect
            (cl-letf (((symbol-function 's3-publish--read-profile-name)
                       (lambda () (error "Prompted for a profile"))))
              (setq s3-publish-on-save-profile "test")
              (s3-publish-on-save-mode 1)
              (should (memq #'s3-publish--republish-on-save after-save-hook))
              (s3-publish-on-save-mode -1)
              (should-not (memq #'s3-publish--republish-on-save after-save-hook)))
          (kill-buffer))))))

;;; Signed URLs

(ert-deftest s3-publish-test-parse-duration ()
  "Durations are seconds, or a number with a unit."
  (should (= (s3-publish--parse-duration "90") 90))
  (should (= (s3-publish--parse-duration "90s") 90))
  (should (= (s3-publish--parse-duration "30m") 1800))
  (should (= (s3-publish--parse-duration " 12h ") 43200))
  (should (= (s3-publish--parse-duration "7d") 604800))
  (should (= (s3-publish--parse-duration "2w") 1209600))
  (should (= (s3-publish--parse-duration 60) 60))
  (should-error (s3-publish--parse-duration "soon") :type 'user-error)
  (should-error (s3-publish--parse-duration "7y") :type 'user-error))

(ert-deftest s3-publish-test-sign-url-command ()
  "Signing accepts a key, a URL or an s3:// URI, and copies an https URL."
  (test-s3-publish-with-fake-s3
    (dolist (object '("k.html" "/k.html" "s3://bkt/k.html"
                      "https://bkt.fake-endpoint/k.html"))
      (let ((url (s3-publish-sign-url "test" object "1h")))
        (should (equal url "https://bkt.fake-endpoint/k.html?Expires=+3600&Signature=fake"))
        (should (equal (car kill-ring) url))))
    (should-error (s3-publish-sign-url "test" "s3://other/k.html" "1h"))))

(ert-deftest s3-publish-test-signed-url-is-https ()
  "s3cmd prints http:// URLs; the signed URL handed out is https."
  (cl-letf (((symbol-function 's3-publish--s3cmd-or-error)
             (lambda (&rest _)
               "http://bkt.host/k.txt?AWSAccessKeyId=A&Expires=1&Signature=x%3D\n")))
    (should (equal (s3-publish--signed-url test-s3-publish--profile "k.txt" 60)
                   "https://bkt.host/k.txt?AWSAccessKeyId=A&Expires=1&Signature=x%3D"))))

(ert-deftest s3-publish-test-private-profile-hands-out-s3-uri ()
  "A private profile without an expiry hands out the s3:// URI."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (copy-sequence test-s3-publish--profile)
                            :public-acl nil))))
      (with-temp-buffer
        (insert "secret")
        (should (string-prefix-p "s3://bkt/" (s3-publish-buffer)))))))

(ert-deftest s3-publish-test-private-profile-hands-out-signed-urls ()
  "With an expiry, a private document and the links in it are signed."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (copy-sequence test-s3-publish--profile)
                            :public-acl nil)))
          (s3-publish-signed-url-expiry "1d"))
      (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n"
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (let ((url (s3-publish-org-buffer)))
                (should (string-match-p "\\`https://.*Expires=\\+86400" url))
                (should (string-match-p
                         (regexp-quote
                          (format "<a href=\"https://bkt.fake-endpoint/%s?Expires=+86400"
                                  (test-s3-publish--key file)))
                         (test-s3-publish--object
                          (test-s3-publish--key file ".html"))))
                ;; Private uploads are never listed in the public feed.
                (should-not (test-s3-publish--object "index.xml")))
            (kill-buffer)))))))

(provide 'test-s3-publish-network)
