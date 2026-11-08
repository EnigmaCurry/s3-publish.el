;;; test-s3-publish.el --- Tests for s3-publish  -*- lexical-binding: t; -*-

(require 'ert)
(require 's3-publish)

(defun test-s3-publish--org (markdown)
  "Convert MARKDOWN and return only its body, without the generated header."
  (let* ((org (s3-publish-markdown-to-org markdown))
         (start (string-match "#\\+OPTIONS: \\^:nil\n\n" org)))
    (string-trim (substring org (match-end 0)))))

(defun test-s3-publish--keyword (markdown keyword)
  "Return the value of KEYWORD in the Org produced from MARKDOWN."
  (let ((org (s3-publish-markdown-to-org markdown)))
    (and (string-match (format "^#\\+%s: \\(.*\\)$" keyword) org)
         (match-string 1 org))))


;;; Headings and title promotion

(ert-deftest s3-publish-test-single-h1-promoted-to-title ()
  "A lone H1 becomes #+TITLE and the rest of the headings shift up."
  (let ((md "# My Document\n\n## Section\n\n### Detail\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "My Document"))
    (should (equal (test-s3-publish--org md) "* Section\n\n** Detail"))))

(ert-deftest s3-publish-test-multiple-h1-not-promoted ()
  "With more than one H1 nothing is promoted and levels are preserved."
  (let ((md "# One\n\n# Two\n"))
    (should-not (test-s3-publish--keyword md "TITLE"))
    (should (equal (test-s3-publish--org md) "* One\n\n* Two"))))

(ert-deftest s3-publish-test-closed-atx-heading ()
  "Trailing hashes on a closed ATX heading are stripped."
  (should (equal (test-s3-publish--org "## Section ##\n\n## Other\n")
                 "** Section\n\n** Other")))

(ert-deftest s3-publish-test-setext-headings ()
  "Setext headings are recognised and normalised."
  (let ((md "Title Here\n==========\n\nSub\n---\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "Title Here"))
    (should (equal (test-s3-publish--org md) "* Sub"))))


;;; Front matter

(ert-deftest s3-publish-test-yaml-frontmatter ()
  "YAML front matter supplies the title and description and is dropped."
  (let ((md "---\ntitle: \"Quoted Title\"\ndescription: A summary\ntags: [a, b]\n---\n\nBody text.\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "Quoted Title"))
    (should (equal (test-s3-publish--keyword md "DESCRIPTION") "A summary"))
    (should (equal (test-s3-publish--org md) "Body text."))))

(ert-deftest s3-publish-test-unclosed-frontmatter-is-content ()
  "An unterminated front matter fence is left alone as content."
  (should (equal (test-s3-publish--org "---\nnot front matter\n") "-----\nnot front matter")))

(ert-deftest s3-publish-test-frontmatter-title-blocks-promotion ()
  "An explicit front matter title keeps H1 as a body heading."
  (let ((md "---\ntitle: Real Title\n---\n\n# Section\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "Real Title"))
    (should (equal (test-s3-publish--org md) "* Section"))))


;;; Inline markup

(ert-deftest s3-publish-test-emphasis ()
  "Bold, italic, bold-italic and strikethrough map onto Org markers."
  (should (equal (test-s3-publish--org "a **bold** b\n") "a *bold* b"))
  (should (equal (test-s3-publish--org "a __bold__ b\n") "a *bold* b"))
  (should (equal (test-s3-publish--org "a *ital* b\n") "a /ital/ b"))
  (should (equal (test-s3-publish--org "a _ital_ b\n") "a /ital/ b"))
  (should (equal (test-s3-publish--org "a ***both*** b\n") "a */both/* b"))
  (should (equal (test-s3-publish--org "a ~~gone~~ b\n") "a +gone+ b")))

(ert-deftest s3-publish-test-bold-is-not-reparsed-as-italic ()
  "Converting bold must not leave markers the italic pass can re-match."
  (should (equal (test-s3-publish--org "**x** and *y*\n") "*x* and /y/")))

(ert-deftest s3-publish-test-snake-case-preserved ()
  "Underscores inside identifiers are not emphasis."
  (should (equal (test-s3-publish--org "call some_function_name now\n")
                 "call some_function_name now")))

(ert-deftest s3-publish-test-inline-code ()
  "Inline code becomes verbatim and its contents are never reinterpreted."
  (should (equal (test-s3-publish--org "use `a *b* c` here\n") "use =a *b* c= here"))
  (should (equal (test-s3-publish--org "use ``a `b` c`` here\n") "use =a `b` c= here")))

(ert-deftest s3-publish-test-links-and-images ()
  "Links, images and autolinks map onto Org link syntax."
  (should (equal (test-s3-publish--org "see [the docs](https://example.com/x)\n")
                 "see [[https://example.com/x][the docs]]"))
  (should (equal (test-s3-publish--org "![alt text](img.png)\n") "[[img.png]]"))
  (should (equal (test-s3-publish--org "<https://example.com>\n")
                 "[[https://example.com]]"))
  (should (equal (test-s3-publish--org "[t](https://e.com \"title\")\n")
                 "[[https://e.com][t]]")))

(ert-deftest s3-publish-test-backslash-escapes ()
  "Backslash-escaped punctuation is emitted literally, not as emphasis."
  (should (equal (test-s3-publish--org "a \\*not bold\\* b\n") "a *not bold* b"))
  (should (equal (test-s3-publish--org "2 \\* 3\n") "2 * 3")))

(ert-deftest s3-publish-test-footnotes ()
  "Footnote references and definitions become Org footnotes."
  (should (equal (test-s3-publish--org "text[^1]\n\n[^1]: the note\n")
                 "text[fn:1]\n\n[fn:1] the note")))


;;; Code blocks

(ert-deftest s3-publish-test-fenced-src-block ()
  "A fenced block with a language becomes a src block."
  (should (equal (test-s3-publish--org "```python\nx = 1\n```\n")
                 "#+begin_src python\nx = 1\n#+end_src")))

(ert-deftest s3-publish-test-fenced-example-block ()
  "A fenced block without a language becomes an example block."
  (should (equal (test-s3-publish--org "```\nplain\n```\n")
                 "#+begin_example\nplain\n#+end_example")))

(ert-deftest s3-publish-test-tilde-fence ()
  "Tilde fences are supported."
  (should (equal (test-s3-publish--org "~~~sh\nls\n~~~\n")
                 "#+begin_src sh\nls\n#+end_src")))

(ert-deftest s3-publish-test-code-block-contents-not-converted ()
  "Markdown inside a code block is passed through untouched."
  (should (equal (test-s3-publish--org "```js\nconst a = `x`; // **bold** _it_\n```\n")
                 "#+begin_src js\nconst a = `x`; // **bold** _it_\n#+end_src")))

(ert-deftest s3-publish-test-code-block-org-markup-escaped ()
  "Lines a code block shares with Org syntax are comma-escaped."
  (should (equal (test-s3-publish--org "```\n* star\n#+keyword\n```\n")
                 "#+begin_example\n,* star\n,#+keyword\n#+end_example")))

(ert-deftest s3-publish-test-unterminated-fence-is-closed ()
  "An unclosed fence is still terminated so Org does not swallow the rest."
  (should (equal (test-s3-publish--org "```sh\nls\n")
                 "#+begin_src sh\nls\n#+end_src")))

(ert-deftest s3-publish-test-heading-inside-fence-ignored ()
  "A # line inside a fence is not a heading and cannot trigger promotion."
  (let ((md "## Real\n\n```\n# not a heading\n```\n"))
    (should-not (test-s3-publish--keyword md "TITLE"))
    (should (equal (test-s3-publish--org md)
                   "** Real\n\n#+begin_example\n# not a heading\n#+end_example"))))


;;; Lists

(ert-deftest s3-publish-test-unordered-list-bullets-normalised ()
  "All Markdown bullets become \"-\" so Org cannot read them as headings."
  (should (equal (test-s3-publish--org "* one\n* two\n") "- one\n- two"))
  (should (equal (test-s3-publish--org "+ one\n+ two\n") "- one\n- two"))
  (should (equal (test-s3-publish--org "- one\n- two\n") "- one\n- two")))

(ert-deftest s3-publish-test-nested-list-indentation-kept ()
  "Nesting indentation is preserved."
  (should (equal (test-s3-publish--org "- one\n  - nested\n    - deeper\n")
                 "- one\n  - nested\n    - deeper")))

(ert-deftest s3-publish-test-ordered-list ()
  "Ordered lists keep their numbers and accept both . and ) delimiters."
  (should (equal (test-s3-publish--org "1. one\n2. two\n") "1. one\n2. two"))
  (should (equal (test-s3-publish--org "1) one\n") "1. one")))

(ert-deftest s3-publish-test-task-list ()
  "Task list checkboxes map onto Org checkboxes."
  (should (equal (test-s3-publish--org "- [ ] todo\n- [x] done\n")
                 "- [ ] todo\n- [X] done")))

(ert-deftest s3-publish-test-list-item-inline-markup ()
  "Inline markup inside list items is converted."
  (should (equal (test-s3-publish--org "- a **b** `c`\n") "- a *b* =c="))) 


;;; Blocks

(ert-deftest s3-publish-test-blockquote ()
  "A blockquote run is wrapped and its contents converted."
  (should (equal (test-s3-publish--org "> quoted **text**\n> more\n")
                 "#+begin_quote\nquoted *text*\nmore\n#+end_quote")))

(ert-deftest s3-publish-test-nested-blockquote ()
  "Nested blockquotes nest."
  (should (equal (test-s3-publish--org "> outer\n> > inner\n")
                 "#+begin_quote\nouter\n#+begin_quote\ninner\n#+end_quote\n#+end_quote")))

(ert-deftest s3-publish-test-thematic-break ()
  "Thematic breaks become Org rules, and are not mistaken for bullets."
  (should (equal (test-s3-publish--org "a\n\n***\n\nb\n") "a\n\n-----\n\nb"))
  (should (equal (test-s3-publish--org "a\n\n___\n\nb\n") "a\n\n-----\n\nb")))

(ert-deftest s3-publish-test-table ()
  "Tables keep their rows and gain an Org rule row."
  (should (equal (test-s3-publish--org "| a | b |\n|---|:-:|\n| 1 | 2 |\n")
                 "| a | b |\n|---+---|\n| 1 | 2 |")))

(ert-deftest s3-publish-test-leading-bold-not-a-heading ()
  "A paragraph starting with bold is indented so Org sees text, not a heading."
  (should (equal (s3-publish-markdown-to-org "**Note:** hello\n")
                 "#+OPTIONS: ^:nil\n\n *Note:* hello\n")))


;;; End-to-end

(ert-deftest s3-publish-test-exports-through-org-exporter ()
  "Converted Markdown renders via Org's HTML exporter, matching Org exports."
  (let* ((html-file (s3-publish--org-string-export-to-html
                     (s3-publish-markdown-to-org
                      "# Doc Title\n\n## Section\n\nSome **bold** text.\n")))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
      ;; Org's own title, content wrapper and outline classes must be present,
      ;; which is what makes the styling identical to a published Org buffer.
      (should (string-match-p "<h1 class=\"title\">Doc Title</h1>" html))
      (should (string-match-p "<div id=\"content\" class=\"content\">" html))
      (should (string-match-p "class=\"outline-2\"" html))
      (should (string-match-p "<b>bold</b>" html))
      ;; Org's default stylesheet is embedded, so the page is self-contained.
      (should (string-match-p "<style type=\"text/css\">" html))
      (should (string-match-p "#content { max-width: 60em" html))
      (delete-file html-file)))

(ert-deftest s3-publish-test-markdown-buffer-p ()
  "Markdown buffers are recognised by mode or by file extension."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/x.md")
    (should (s3-publish--markdown-buffer-p))
    (setq buffer-file-name "/tmp/x.markdown")
    (should (s3-publish--markdown-buffer-p))
    (setq buffer-file-name "/tmp/x.txt")
    (should-not (s3-publish--markdown-buffer-p))
    (setq buffer-file-name nil)))

(ert-deftest s3-publish-test-document-keys-share-a-base ()
  "A document's source and HTML objects differ only in extension."
  (let* ((salt "salt")
         (md-key (s3-publish-get-file-key "/tmp/doc.md" salt))
         (base (file-name-sans-extension md-key)))
    (should (equal (file-name-extension md-key) "md"))
    (should (equal (concat base ".html")
                   (concat (file-name-sans-extension md-key) ".html")))))

(provide 'test-s3-publish)
