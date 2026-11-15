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
          (should-not (string-match-p " src=\"shot.png\"" html))
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

(ert-deftest s3-publish-test-api-reports-uploaded-images ()
  "An API publish reports the images it uploaded, apart from its own keys."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file org "doc.org" "#+TITLE: D\n\n[[file:big.png]]\n\n[[file:small.png]]\n"
      (let ((big (expand-file-name "big.png" (file-name-directory org)))
            (small (expand-file-name "small.png" (file-name-directory org))))
        (test-s3-publish--write-png big)
        (test-s3-publish--write-png small)
        (let* ((s3-publish-html-inline-image-max-size 10)
               (result (s3-publish-api-publish org "test")))
          (should (equal (plist-get result :images)
                         (list (test-s3-publish--key big)
                               (test-s3-publish--key small))))
          (should (= 2 (length (plist-get result :keys)))))
        ;; Embedded images are not uploaded, so not reported.
        (should-not (plist-get (s3-publish-api-publish org "test") :images))))))

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

(ert-deftest s3-publish-test-private-profile-signs-a-week-by-default ()
  "Out of the box, a private profile hands out a URL signed for a week."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (copy-sequence test-s3-publish--profile)
                            :public-acl nil)))
          (s3-publish-signed-url-expiry
           (eval (car (get 's3-publish-signed-url-expiry 'standard-value)) t)))
      (with-temp-buffer
        (insert "secret")
        (let ((url (s3-publish-buffer)))
          (should (string-prefix-p "https://bkt.fake-endpoint/" url))
          (should (string-match-p "Expires=\\+604800" url))
          (should (equal (car kill-ring) url)))))))

(ert-deftest s3-publish-test-private-profile-hands-out-s3-uri ()
  "A private profile without an expiry hands out the s3:// URI."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (copy-sequence test-s3-publish--profile)
                            :public-acl nil)))
          (s3-publish-signed-url-expiry nil))
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

;;; Listing published files

(ert-deftest s3-publish-test-feed-items ()
  "Feed items parse, CDATA and unescaped titles included."
  (let ((items (s3-publish--feed-items
                "<rss><channel><title>T</title>
    <item>
      <title>Fish & Chips</title>
      <link>https://b.h/1.html</link>
      <guid>https://b.h/1.html</guid>
      <description><![CDATA[Hi <a href=\"https://b.h/1.org\">src</a>]]></description>
      <pubDate>Sat, 14 Nov 2026 22:00:00 -0700</pubDate>
    </item>
    <item><title>Two</title><link>https://b.h/2.txt</link></item>
</channel></rss>")))
    (should (= 2 (length items)))
    (should (equal (plist-get (car items) :title) "Fish & Chips"))
    (should (equal (plist-get (car items) :link) "https://b.h/1.html"))
    (should (equal (plist-get (car items) :description)
                   "Hi <a href=\"https://b.h/1.org\">src</a>"))
    (should (equal (format-time-string "%F %T" (plist-get (car items) :date) t)
                   "2026-11-15 05:00:00"))
    (should-not (plist-get (cadr items) :date))))

(defmacro test-s3-publish-with-list (&rest body)
  "Run BODY in the `s3-publish-list' buffer of the test profile."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-overriding-action '(display-buffer-no-window
                                             (allow-no-window . t))))
     (save-window-excursion
       (s3-publish-list "test")
       (with-current-buffer "*s3-publish: test*"
         (unwind-protect (progn ,@body)
           (kill-buffer))))))

(ert-deftest s3-publish-test-list-shows-published-files ()
  "The list shows each published file, newest first."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file a "a.txt" "a"
      (test-s3-publish-with-file b "b.txt" "b"
        (let* ((profile (s3-publish-get-credentials test-s3-publish--profile))
               (url-a (s3-publish-upload-file a profile "A" "first"))
               (url-b (progn
                        ;; Feed dates have one second resolution.
                        (sleep-for 1.1)
                        (s3-publish-upload-file b profile "B" "second"))))
          (test-s3-publish-with-list
            (should (equal (mapcar #'car tabulated-list-entries)
                           (list (test-s3-publish--key b) (test-s3-publish--key a))))
            (goto-char (point-min))
            (should (equal (tabulated-list-get-id) (test-s3-publish--key b)))
            (s3-publish-list-copy-url)
            (should (equal (car kill-ring) url-b))))))))

(ert-deftest s3-publish-test-list-delete-removes-document-and-source ()
  "Deleting a document from the list deletes its page, source and entry."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n"
      (with-current-buffer (find-file-noselect file)
        (unwind-protect (s3-publish-org-buffer)
          (kill-buffer)))
      (should (test-s3-publish--object (test-s3-publish--key file)))
      (test-s3-publish-with-list
        (goto-char (point-min))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (s3-publish-list-delete))
        (should-not tabulated-list-entries))
      (should-not (test-s3-publish--object (test-s3-publish--key file ".html")))
      (should-not (test-s3-publish--object (test-s3-publish--key file)))
      (should-not (string-match-p "<item>" (test-s3-publish--object "index.xml"))))))

(ert-deftest s3-publish-test-list-without-a-feed-lists-the-bucket ()
  "A profile without an RSS feed is listed from the bucket itself."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (plist-put (copy-sequence test-s3-publish--profile)
                                       :rss-feed nil)
                            :public-acl nil))))
      (test-s3-publish-with-file file "a.txt" "hello"
        (s3-publish-upload-file file (s3-publish-get-credentials
                                      (car s3-publish-profiles)))
        (test-s3-publish-with-list
          (should (eq s3-publish-list--source 'bucket))
          (should (equal (mapcar #'car tabulated-list-entries)
                         (list (test-s3-publish--key file))))
          (should (equal (aref (cadr (car tabulated-list-entries)) 1) "5"))
          ;; Private: copying gives a URL signed for a week, opening one
          ;; signed for an hour.
          (goto-char (point-min))
          (s3-publish-list-copy-url)
          (should (equal (car kill-ring)
                         (concat "https://bkt.fake-endpoint/"
                                 (test-s3-publish--key file)
                                 "?Expires=+604800&Signature=fake")))
          (let (opened)
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url &rest _) (setq opened url))))
              (s3-publish-list-browse))
            (should (string-match-p "\\`https://.*Expires=\\+3600" opened)))
          ;; There is no feed to switch to.
          (should-error (s3-publish-list-toggle-source) :type 'user-error))))))

(ert-deftest s3-publish-test-list-bucket-with-prefix-and-toggle ()
  "A prefix argument lists the whole bucket; `t' switches back to the feed."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "a.txt" "a"
      (s3-publish-upload-file file (s3-publish-get-credentials
                                    test-s3-publish--profile))
      (let ((display-buffer-overriding-action '(display-buffer-no-window
                                                (allow-no-window . t))))
        (save-window-excursion
          (s3-publish-list "test" t)
          (with-current-buffer "*s3-publish: test*"
            (unwind-protect
                (progn
                  (should (eq s3-publish-list--source 'bucket))
                  ;; The bucket holds the feed and index too.
                  (should (equal (sort (mapcar #'car tabulated-list-entries) #'string<)
                                 (sort (list (test-s3-publish--key file)
                                             "index.html" "index.xml")
                                       #'string<)))
                  (s3-publish-list-toggle-source)
                  (should (eq s3-publish-list--source 'feed))
                  (should (equal (mapcar #'car tabulated-list-entries)
                                 (list (test-s3-publish--key file)))))
              (kill-buffer))))))))

(ert-deftest s3-publish-test-list-bucket-delete-takes-document-source ()
  "Deleting a page from the bucket list deletes its source beside it."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profiles
           (list (plist-put (copy-sequence test-s3-publish--profile)
                            :rss-feed nil))))
      (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n"
        (with-current-buffer (find-file-noselect file)
          (unwind-protect (s3-publish-org-buffer)
            (kill-buffer)))
        (test-s3-publish-with-list
          (goto-char (point-min))
          (while (not (equal (tabulated-list-get-id)
                             (test-s3-publish--key file ".html")))
            (forward-line 1))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (s3-publish-list-delete))
          (should-not tabulated-list-entries))))))

(ert-deftest s3-publish-test-bucket-objects-parse-s3cmd-ls ()
  "The bucket listing reads s3cmd's own `ls' format."
  (cl-letf (((symbol-function 's3-publish--s3cmd-or-error)
             (lambda (&rest _)
               (concat "2026-11-14 22:00          1234  s3://bkt/a b.html\n"
                       "2026-11-13 09:05             0  s3://bkt/dir/x.png\n"
                       "                          DIR  s3://bkt/dir/\n"))))
    (let ((objects (s3-publish--bucket-objects test-s3-publish--profile)))
      (should (equal (mapcar (lambda (o) (plist-get o :key)) objects)
                     '("a b.html" "dir/x.png")))
      (should (equal (plist-get (car objects) :size) 1234))
      (should (equal (format-time-string "%F %H:%M" (plist-get (car objects) :date))
                     "2026-11-14 22:00")))))

;;; Programmatic API

(ert-deftest s3-publish-test-api-profiles ()
  "Profiles are listed without credentials, the last used marked."
  (test-s3-publish-with-fake-s3
    (let ((s3-publish-profile-history '("test")))
      (should (equal (s3-publish-api-profiles)
                     '((:name "test" :bucket "bkt" :endpoint "fake-endpoint"
                              :public t :feed t :last-used t)))))))

(ert-deftest s3-publish-test-api-needs-a-profile-and-a-real-file ()
  "No profile, a relative path or a missing file is an error, not a guess."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "a.txt" "a"
      (should-error (s3-publish-api-publish file nil))
      (should-error (s3-publish-api-publish file ""))
      (should-error (s3-publish-api-publish "a.txt" "test"))
      (should-error (s3-publish-api-publish (concat file ".gone") "test"))
      (should-error (s3-publish-api-list nil))
      (should-not (test-s3-publish--log)))))

(ert-deftest s3-publish-test-api-publish-leaves-user-buffers-alone ()
  "Publishing reads the file from disk, not a modified buffer visiting it."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "#+TITLE: Disk\n\nOn disk.\n"
      (let ((visiting (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer visiting
              (goto-char (point-max))
              (insert "Unsaved edit.\n")
              (let* ((buffers (buffer-list))
                     (result (s3-publish-api-publish file "test")))
                (should (equal (plist-get result :keys)
                               (list (test-s3-publish--key file ".html")
                                     (test-s3-publish--key file))))
                (should (equal (plist-get result :url)
                               (concat "https://bkt.fake-endpoint/"
                                       (test-s3-publish--key file ".html"))))
                (should (eq (plist-get result :status) 'done))
                (let ((html (test-s3-publish--object
                             (test-s3-publish--key file ".html"))))
                  (should (string-match-p "On disk" html))
                  (should-not (string-match-p "Unsaved" html))
                  (should (string-match-p "<title>Disk</title>" html)))
                ;; The visiting buffer is still modified, still unsaved.
                (should (buffer-modified-p))
                (should (string-match-p "Unsaved" (buffer-string)))
                (should (equal (with-temp-buffer (insert-file-contents file)
                                                 (buffer-string))
                               "#+TITLE: Disk\n\nOn disk.\n"))
                ;; No buffers left behind, and the kill ring untouched.
                (should (equal (seq-filter #'buffer-live-p buffers)
                               (seq-filter #'buffer-live-p (buffer-list))))
                (should-not kill-ring)))
          (with-current-buffer visiting (set-buffer-modified-p nil))
          (kill-buffer visiting))))))

(ert-deftest s3-publish-test-api-org-title-defaults-to-file-name ()
  "An Org file without a title is titled after the file, as Org would."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "my-notes.org" "Just text.\n"
      (s3-publish-api-publish file "test")
      (should (string-match-p "<title>my-notes</title>"
                              (test-s3-publish--object
                               (test-s3-publish--key file ".html")))))))

(ert-deftest s3-publish-test-api-markdown-and-plain-files ()
  "Markdown publishes as a document, other files as themselves."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Title\n\nBody.\n"
      (let ((result (s3-publish-api-publish md "test")))
        (should (= 2 (length (plist-get result :keys))))
        (should (string-match-p "<title>Title</title>"
                                (test-s3-publish--object
                                 (test-s3-publish--key md ".html"))))))
    (test-s3-publish-with-file txt "a.txt" "plain"
      (let ((result (s3-publish-api-publish txt "test" "Custom title")))
        (should (equal (plist-get result :keys) (list (test-s3-publish--key txt))))
        (should (equal (test-s3-publish--object (test-s3-publish--key txt)) "plain"))
        (should (string-match-p "<title>Custom title</title>"
                                (test-s3-publish--object "index.xml")))))))

(ert-deftest s3-publish-test-api-async-status ()
  "A background publish reports its job, which can be polled until done."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (test-s3-publish-with-file file "a.txt" "a"
        (let* ((result (s3-publish-api-publish file "test"))
               (job (plist-get result :job)))
          (should (memq (plist-get result :status) '(running queued)))
          (should (equal (plist-get (plist-get (s3-publish-api-status) :running) :job)
                         job))
          (should (s3-publish--wait 30))
          (should (eq (plist-get (s3-publish-api-status job) :status) 'done))
          (should (eq (plist-get (s3-publish-api-status 99999) :status) 'unknown)))))))

(ert-deftest s3-publish-test-api-async-failure-and-superseded ()
  "Failures carry their error; a replaced waiting job says what replaced it."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-async
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (let ((process-environment (cons "FAKE_S3_FAIL=1" process-environment)))
          (test-s3-publish-with-file a "a.txt" "a"
            (test-s3-publish-with-file b "b.txt" "b"
              (let* ((first (plist-get (s3-publish-api-publish a "test") :job))
                     (old (plist-get (s3-publish-api-publish b "test") :job))
                     (new (plist-get (s3-publish-api-publish b "test") :job)))
                (should (eq (plist-get (s3-publish-api-status old) :status)
                            'superseded))
                (should (equal (plist-get (s3-publish-api-status old) :superseded-by)
                               new))
                (should (s3-publish--wait 30))
                (let ((status (s3-publish-api-status first)))
                  (should (eq (plist-get status :status) 'failed))
                  (should (string-match-p "simulated failure"
                                          (plist-get status :error))))))))))))

(ert-deftest s3-publish-test-api-list ()
  "Listing gives the feed's entries, or the bucket's objects."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n"
      (s3-publish-api-publish file "test")
      (let ((feed (s3-publish-api-list "test")))
        (should (= 1 (length feed)))
        (should (equal (plist-get (car feed) :title) "Doc"))
        (should (equal (plist-get (car feed) :key) (test-s3-publish--key file ".html"))))
      (should (equal (sort (mapcar (lambda (o) (plist-get o :key))
                                   (s3-publish-api-list "test" t))
                           #'string<)
                     (sort (list (test-s3-publish--key file ".html")
                                 (test-s3-publish--key file)
                                 "index.html" "index.xml")
                           #'string<))))))

(ert-deftest s3-publish-test-api-delete-document-by-url-path-or-source ()
  "Deleting a document by any of its names removes page, source and entry."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "#+TITLE: Doc\n"
      (let ((both (list (test-s3-publish--key file ".html")
                        (test-s3-publish--key file))))
        (dolist (target (list (lambda (r) (plist-get r :url))
                              (lambda (_) file)
                              (lambda (_) (concat "s3://bkt/"
                                                  (test-s3-publish--key file)))))
          (let ((result (s3-publish-api-publish file "test")))
            (should (equal (sort (s3-publish-api-delete (funcall target result) "test")
                                 #'string<)
                           (sort (copy-sequence both) #'string<)))
            (dolist (key both)
              (should-not (test-s3-publish--object key)))
            (should-not (string-match-p "<item>"
                                        (test-s3-publish--object "index.xml")))))
        ;; Nothing left to delete is not an error.
        (should-not (s3-publish-api-delete file "test"))))))

(ert-deftest s3-publish-test-api-delete-plain-file-keeps-others ()
  "Deleting a plain file deletes only it."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file a "a.txt" "a"
      (test-s3-publish-with-file b "b.txt" "b"
        (s3-publish-api-publish a "test")
        (s3-publish-api-publish b "test")
        (should (equal (s3-publish-api-delete a "test")
                       (list (test-s3-publish--key a))))
        (should (test-s3-publish--object (test-s3-publish--key b)))))))

(ert-deftest s3-publish-test-api-sign ()
  "Signing a document's path signs its HTML page."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "doc.org" "x"
      (should (string-match-p
               (regexp-quote (concat (test-s3-publish--key file ".html")
                                     "?Expires=+3600"))
               (s3-publish-api-sign file "1h" "test")))
      (should-not kill-ring))))

;;; Running code when publishing

(defconst test-s3-publish--code-doc
  "#+TITLE: Code
#+S3_PUBLISH_TANGLE: t

#+begin_src shell :eval yes :exports both :results output
  echo evaluated-$((6 * 7))
  touch ran.flag
#+end_src

#+begin_src text :tangle tangled.txt
tangled content
#+end_src
"
  "An Org document that evaluates a block on export and asks to be tangled.")

(ert-deftest s3-publish-test-api-runs-no-code-by-default ()
  "Without RUN-CODE nothing is evaluated or tangled; code still exports."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "code.org" test-s3-publish--code-doc
      (let ((org-confirm-babel-evaluate
             (lambda (&rest _) (error "Asked to evaluate"))))
        (s3-publish-api-publish file "test"))
      (let ((html (test-s3-publish--object (test-s3-publish--key file ".html")))
            (dir (file-name-directory file)))
        (should (string-match-p "echo evaluated" html))
        (should-not (string-match-p "evaluated-42" html))
        (should-not (file-exists-p (expand-file-name "ran.flag" dir)))
        (should-not (file-exists-p (expand-file-name "tangled.txt" dir)))))))

(ert-deftest s3-publish-test-api-run-code-tangles-and-evaluates ()
  "With RUN-CODE the document is tangled, and evaluated without asking."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file file "code.org" test-s3-publish--code-doc
      (let ((org-confirm-babel-evaluate t))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "Asked a question")))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (error "Asked a question"))))
          (s3-publish-api-publish file "test" nil t)))
      (let ((html (test-s3-publish--object (test-s3-publish--key file ".html")))
            (dir (file-name-directory file)))
        (should (string-match-p "evaluated-42" html))
        (should (file-exists-p (expand-file-name "ran.flag" dir)))
        (should (equal (with-temp-buffer
                         (insert-file-contents (expand-file-name "tangled.txt" dir))
                         (buffer-string))
                       "tangled content\n"))))))


;;; The skill's batch driver

(defconst test-s3-publish--s3p
  (file-truename
   (expand-file-name "../../claude-skill/s3-publish/s3p" test-s3-publish--bin))
  "The skill's command line wrapper.")

(defun test-s3-publish--s3p (&rest args)
  "Run s3p with ARGS in a sandbox HOME; return (EXIT . parsed JSON)."
  (with-temp-buffer
    (let* ((default-directory (file-truename temporary-file-directory))
           (exit (apply #'call-process test-s3-publish--s3p nil
                       (list t nil) nil args)))
      (goto-char (point-min))
      (cons exit (json-parse-buffer :object-type 'alist :null-object nil
                                    :false-object nil)))))

(defmacro test-s3-publish-with-s3p-home (&rest body)
  "Run BODY with a sandbox HOME holding s3-publish settings and credentials."
  (declare (indent 0) (debug t))
  `(let* ((home (make-temp-file "s3p-home-" t))
          (process-environment
           (append (list (concat "HOME=" home)
                         "S3P_SETTINGS_FILE"
                         "S3P_HISTORY_FILE"
                         "S3P_LOAD_PATH")
                   process-environment)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".emacs.d" home))
           (with-temp-file (expand-file-name ".emacs.d/custom.el" home)
             (insert ";; Other settings must not be evaluated:\n"
                     "(error \"evaluated the custom file\")\n"
                     (format "(custom-set-variables\n '(other-package-option (error \"no\"))\n '(s3-publish-profiles '%S)\n '(s3-publish-cache-control \"max-age=7\"))\n"
                             (list test-s3-publish--profile))))
           (with-temp-file (expand-file-name ".authinfo" home)
             (insert "machine s3-publish-test login AK password SK\n"))
           ,@body)
       (delete-directory home t))))

(ert-deftest s3-publish-test-s3p-publish-list-delete ()
  "The skill publishes, lists and deletes through a batch Emacs."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-s3p-home
      (test-s3-publish-with-file file "doc.org" "#+TITLE: Via s3p\n\nHi.\n"
        (let ((check (test-s3-publish--s3p "check")))
          (should (= 0 (car check)))
          (should (equal (alist-get 'profiles (cdr check)) 1)))
        (let ((profiles (test-s3-publish--s3p "profiles")))
          (should (equal (alist-get 'name (aref (cdr profiles) 0)) "test")))
        (let ((published (test-s3-publish--s3p "publish" file "test")))
          (should (= 0 (car published)))
          (should (equal (alist-get 'url (cdr published))
                         (concat "https://bkt.fake-endpoint/"
                                 (test-s3-publish--key file ".html")))))
        (should (string-match-p "Via s3p" (test-s3-publish--object
                                           (test-s3-publish--key file ".html"))))
        ;; Settings beyond the profiles came through too.
        (should (member "--add-header=Cache-Control: max-age=7"
                        (test-s3-publish--object-flags
                         (test-s3-publish--key file ".html"))))
        (let ((listed (test-s3-publish--s3p "list" "test")))
          (should (equal (alist-get 'title (aref (cdr listed) 0)) "Via s3p")))
        (let ((deleted (test-s3-publish--s3p "delete" file "test")))
          (should (= 2 (length (alist-get 'deleted (cdr deleted))))))
        (should-not (test-s3-publish--object (test-s3-publish--key file ".html")))))))

(ert-deftest s3-publish-test-s3p-failure-is-json ()
  "A failure exits non-zero with the error as JSON."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-s3p-home
      (let ((result (test-s3-publish--s3p "list" "no-such-profile")))
        (should (= 1 (car result)))
        (should (string-match-p "no-such-profile" (alist-get 'error (cdr result))))))))

;;; Editing published files live

(defmacro test-s3-publish-editing (var target &rest body)
  "Open TARGET with `s3-publish-edit' as the test profile, VAR the buffer.
Kills the buffer afterwards, discarding changes."
  (declare (indent 2) (debug t))
  `(let ((,var (save-window-excursion (s3-publish-edit "test" ,target))))
     (unwind-protect
         (with-current-buffer ,var ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var)))))

(defun test-s3-publish--count (regexp string)
  "Return how many times REGEXP matches in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun test-s3-publish--bucket-keys ()
  "Return the sorted keys in the fake bucket."
  (let ((dir (expand-file-name "objects/bkt" test-s3-publish--root)))
    (sort (seq-remove (lambda (f) (string-suffix-p ".args" f))
                      (directory-files dir nil "\\`[^.]"))
          #'string<)))

(ert-deftest s3-publish-test-edit-document-republishes-to-same-url ()
  "Editing a page edits its source and republishes it at the same URL."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file org "doc.org" "#+TITLE: Doc\n\nOriginal.\n"
      (let* ((url (s3-publish-api-publish org "test"))
             (url (plist-get url :url))
             (keys (test-s3-publish--bucket-keys)))
        (test-s3-publish-editing buffer url
          ;; The source, not the page, in a temporary file of its own.
          (should (equal (buffer-string) "#+TITLE: Doc\n\nOriginal.\n"))
          (should (derived-mode-p 'org-mode))
          (should s3-publish-on-save-mode)
          (should-not (equal buffer-file-name org))
          (let ((dir (plist-get s3-publish--edit-session :dir)))
            (should (file-in-directory-p buffer-file-name dir))
            (test-s3-publish--edit-and-save "#+TITLE: Doc\n\nEdited live.\n")
            (should (string-match-p "Edited live"
                                    (test-s3-publish--object
                                     (test-s3-publish--key org ".html"))))
            (should (equal (test-s3-publish--object (test-s3-publish--key org))
                           "#+TITLE: Doc\n\nEdited live.\n"))
            ;; Same objects, same single feed entry, same URL.
            (should (equal (test-s3-publish--bucket-keys) keys))
            (let ((feed (test-s3-publish--object "index.xml")))
              (should (= 1 (test-s3-publish--count "<item>" feed)))
              (should (string-match-p (regexp-quote url) feed)))
            (kill-buffer buffer)
            (should-not (file-exists-p dir))))
        ;; The original on disk is untouched.
        (should (equal (with-temp-buffer (insert-file-contents org) (buffer-string))
                       "#+TITLE: Doc\n\nOriginal.\n"))))))

(ert-deftest s3-publish-test-edit-keeps-published-images ()
  "An edit session keeps the images the page was published with."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file org "doc.org"
        "#+TITLE: D\n\n[[file:img/big.png]]\n\n[[file:small.png]]\n"
      (let ((dir (file-name-directory org)))
        (test-s3-publish--write-png (expand-file-name "img/big.png" dir))
        ;; Padded past the limit below, so it is uploaded, not embedded.
        (let ((coding-system-for-write 'binary))
          (write-region (make-string 5000 0) nil
                        (expand-file-name "img/big.png" dir) t 'quiet))
        (test-s3-publish--write-png (expand-file-name "small.png" dir))
        (let* ((s3-publish-html-inline-image-max-size 4500)
               (result (s3-publish-api-publish org "test")))
          (with-temp-file (expand-file-name "small.png" dir) (insert "x"))
          (test-s3-publish-editing buffer (car (plist-get result :keys))
            (test-s3-publish--edit-and-save
             "#+TITLE: D\n\nNew text.\n\n[[file:img/big.png]]\n\n[[file:small.png]]\n")
            (let ((html (test-s3-publish--object
                         (test-s3-publish--key org ".html"))))
              (should (string-match-p "New text" html))
              ;; The uploaded image is still linked...
              (should (string-match-p
                       (regexp-quote (car (plist-get result :images))) html))
              ;; ...and the embedded one still embedded.
              (should (string-match-p
                       "src=\"data:image/png;base64,[^\"]+\" data-s3-publish-src=\"small.png\""
                       html))
              (should-not (string-match-p " src=\"\\(img/big\\|small\\).png\"" html)))))))))

(ert-deftest s3-publish-test-edit-markdown-and-plain-files ()
  "Markdown sources and plain files are edited in place the same way."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file md "doc.md" "# Title\n\nFirst.\n"
      (s3-publish-api-publish md "test")
      (test-s3-publish-editing buffer (test-s3-publish--key md ".html")
        (should (equal (buffer-string) "# Title\n\nFirst.\n"))
        (test-s3-publish--edit-and-save "# Title\n\nSecond.\n")
        (should (string-match-p "Second" (test-s3-publish--object
                                          (test-s3-publish--key md ".html"))))))
    (test-s3-publish-with-file txt "notes.txt" "one"
      (let ((url (plist-get (s3-publish-api-publish txt "test") :url)))
        (test-s3-publish-editing buffer url
          (test-s3-publish--edit-and-save "two")
          (should (equal (test-s3-publish--object (test-s3-publish--key txt)) "two"))
          ;; One entry for this file, and the Markdown one still there.
          (let ((feed (test-s3-publish--object "index.xml")))
            (should (= 1 (test-s3-publish--count (regexp-quote (concat "<link>" url "</link>")) feed)))
            (should (= 2 (test-s3-publish--count "<item>" feed)))))))))

(ert-deftest s3-publish-test-edit-refuses-pages-without-source ()
  "A page with no source in the bucket, like the index, cannot be edited."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file txt "a.txt" "a"
      (s3-publish-api-publish txt "test")
      (should-error (s3-publish-edit "test" "index.html") :type 'user-error)
      (should-error (s3-publish-edit "test" "nothing-here.txt") :type 'user-error))))

(ert-deftest s3-publish-test-edit-reuses-an-open-session ()
  "Editing the same file twice returns the buffer already open."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file txt "a.txt" "a"
      (let ((key (test-s3-publish--key txt)))
        (s3-publish-api-publish txt "test")
        (test-s3-publish-editing buffer key
          (should (eq (save-window-excursion (s3-publish-edit "test" key))
                      buffer)))))))

(ert-deftest s3-publish-test-list-edit-key ()
  "`e' in the list edits the entry at point."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file org "doc.org" "#+TITLE: Doc\n"
      (s3-publish-api-publish org "test")
      (test-s3-publish-with-list
        (goto-char (point-min))
        (should (eq (lookup-key s3-publish-list-mode-map "e") #'s3-publish-list-edit))
        (let ((buffer (save-window-excursion (s3-publish-list-edit))))
          (unwind-protect
              (should (equal (plist-get (buffer-local-value 's3-publish--edit-session
                                                            buffer)
                                        :key)
                             (test-s3-publish--key org)))
            (kill-buffer buffer)))))))

;;; Feed and index integrity

(ert-deftest s3-publish-test-republish-keeps-other-feed-entries ()
  "Republishing one file replaces its entry and no other.
This used to wipe every feed item from the first one up to the
republished one: a lazy regexp matched across item boundaries."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file a "a.txt" "a"
      (test-s3-publish-with-file b "b.txt" "b"
        (test-s3-publish-with-file c "c.txt" "c"
          (dolist (file (list a b c))
            (s3-publish-api-publish file "test" (file-name-base file)))
          (cl-flet ((check ()
                      (let ((feed (test-s3-publish--object "index.xml"))
                            (index (test-s3-publish--object "index.html")))
                        (should (= 3 (test-s3-publish--count "<item>" feed)))
                        (should (= 3 (test-s3-publish--count "<div class=\"entry\">" index)))
                        (dolist (file (list a b c))
                          (let ((url (concat "https://bkt.fake-endpoint/"
                                             (test-s3-publish--key file))))
                            (should (= 1 (test-s3-publish--count
                                          (regexp-quote (concat "<link>" url "</link>"))
                                          feed)))
                            (should (= 1 (test-s3-publish--count
                                          (regexp-quote (concat "<h2><a href=\"" url "\""))
                                          index))))))))
            (check)
            ;; The newest, last in the feed and first in the index.
            (s3-publish-api-publish c "test" "c again")
            (check)
            (should (string-match-p "<title>c again</title>"
                                    (test-s3-publish--object "index.xml")))
            ;; The oldest, first in the feed and last in the index.
            (s3-publish-api-publish a "test" "a again")
            (check)
            ;; And one in the middle of both.
            (s3-publish-api-publish b "test" "b again")
            (check)
            (should (string-match-p ">b again</a>" (test-s3-publish--object "index.html")))))))))

(ert-deftest s3-publish-test-feed-item-bounds-stay-in-one-item ()
  "The item found for a URL is that item alone, even when it is last."
  (with-temp-buffer
    (insert "<rss><channel>\n"
            "<item><title>A</title><link>https://h/a</link></item>\n"
            "<item><title>B</title><link>https://h/b</link></item>\n"
            "<item><title>C</title><link>https://h/c</link></item>\n"
            "</channel></rss>")
    (let ((bounds (s3-publish--feed-item-bounds "https://h/c")))
      (should (equal (buffer-substring (nth 0 bounds) (nth 1 bounds))
                     "<item><title>C</title><link>https://h/c</link></item>")))
    (should-not (s3-publish--feed-item-bounds "https://h/z"))
    ;; A URL that merely starts another's is not a match.
    (should-not (s3-publish--feed-item-bounds "https://h/"))))

(ert-deftest s3-publish-test-rebuild-feed-from-index ()
  "A feed that lost items is rebuilt from the HTML index."
  (test-s3-publish-with-fake-s3
    (test-s3-publish-with-file a "a.txt" "a"
      (test-s3-publish-with-file b "b.txt" "b"
        (s3-publish-api-publish a "test" "Title A")
        (s3-publish-api-publish b "test" "Title <b>B</b>")
        ;; Lose every item, as the old republish bug did.
        (let* ((profile (s3-publish-get-credentials test-s3-publish--profile))
               (feed (test-s3-publish--object "index.xml")))
          (s3-publish--put-string
           profile
           (replace-regexp-in-string "<item>\\(?:.\\|\n\\)*?</item>" "" feed)
           "index.xml" "application/rss+xml"))
        (should (= 0 (test-s3-publish--count "<item>" (test-s3-publish--object "index.xml"))))
        (should (= 2 (s3-publish-rebuild-feed "test")))
        (let ((items (s3-publish--feed-items (test-s3-publish--object "index.xml"))))
          ;; Oldest first, as publishing appends them.
          (should (equal (mapcar (lambda (i) (plist-get i :title)) items)
                         '("Title A" "Title <b>B</b>")))
          (should (equal (plist-get (car items) :link)
                         (concat "https://bkt.fake-endpoint/" (test-s3-publish--key a))))
          (should (plist-get (car items) :date))
          (should (string-match-p "published on" (downcase (plist-get (car items) :description)))))
        ;; The channel header is kept.
        (should (string-match-p "<title>T</title>" (test-s3-publish--object "index.xml")))
        ;; Rebuilding again changes nothing but the build date.
        (should (= 2 (s3-publish-rebuild-feed "test")))
        (should (= 2 (test-s3-publish--count "<item>" (test-s3-publish--object "index.xml"))))))))

(provide 'test-s3-publish-network)
