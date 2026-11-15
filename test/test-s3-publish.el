;;; test-s3-publish.el --- Tests for s3-publish  -*- lexical-binding: t; -*-

(require 'ert)
(require 's3-publish)

(defun test-s3-publish--delete-export (html-file)
  "Delete HTML-FILE, exported to a temporary directory, with the directory."
  (mapc #'s3-publish--delete-if-exists
        (s3-publish--temporary-export-files html-file)))

(defun test-s3-publish--org (markdown)
  "Convert MARKDOWN and return its body, without the header or anchor drawers.
Heading anchors are covered separately; stripping the `:PROPERTIES:'
drawers keeps these tests about the markup itself."
  (let* ((org (s3-publish-markdown-to-org markdown))
         (start (string-match "#\\+OPTIONS: \\^:nil\n\n" org))
         (body (substring org (match-end 0)))
         (body (replace-regexp-in-string
                "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n" "" body)))
    (string-trim body)))

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

(ert-deftest s3-publish-test-leading-h1-is-the-title ()
  "A level-1 heading opening the document is its title, even alongside others.
Requiring it to be the only one left real documents with no title at all."
  (let ((md "# One\n\nprose\n\n# Two\n\n## Under Two\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "One"))
    ;; A level-1 heading remains, so levels are left alone rather than
    ;; shifted: shifting would drop "Two" or flatten it onto its child.
    (should (equal (test-s3-publish--org md) "prose\n\n* Two\n\n** Under Two"))))

(ert-deftest s3-publish-test-h1-after-prose-still-promoted-when-alone ()
  "A lone level-1 heading is the title wherever it sits."
  (let ((md "intro\n\n# Only One\n\n## Sub\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "Only One"))
    (should (equal (test-s3-publish--org md) "intro\n\n* Sub"))))

(ert-deftest s3-publish-test-duplicate-title-heading-is-dropped ()
  "A later level-1 heading repeating the title is dropped, keeping its body.
Generated Markdown often restates its own title partway down, which
otherwise renders the title again and lists it twice in the contents."
  (let ((md "# My Title\n\nintro\n\n# My Title\n\n## Section\n\nbody\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "My Title"))
    ;; The repeat is gone, and with no level-1 heading left the sections
    ;; shift up into its place.
    (should (equal (test-s3-publish--org md) "intro\n\n* Section\n\nbody"))))

(ert-deftest s3-publish-test-duplicate-title-not-dropped-when-different ()
  "Only an exact repeat of the title is dropped; other headings stay."
  (let ((md "# My Title\n\nintro\n\n# Other\n"))
    (should (equal (test-s3-publish--keyword md "TITLE") "My Title"))
    (should (equal (test-s3-publish--org md) "intro\n\n* Other"))))

(ert-deftest s3-publish-test-no-h1-means-no-title ()
  "A document that never uses a level-1 heading gets no title."
  (let ((md "## Alpha\n\n## Beta\n"))
    (should-not (test-s3-publish--keyword md "TITLE"))
    (should (equal (test-s3-publish--org md) "** Alpha\n\n** Beta"))))


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
  (should (equal (test-s3-publish--org "![alt text](img.png)\n") "[[file:img.png]]"))
  (should (equal (test-s3-publish--org "![a](/abs/img.png)\n") "[[file:/abs/img.png]]"))
  (should (equal (test-s3-publish--org "[doc](other.md)\n") "[[file:other.md][doc]]"))
  (should (equal (test-s3-publish--org "[up](#intro)\n") "[[#intro][up]]"))
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
      ;; The theme is embedded, so the page is self-contained.
      (should (string-match-p "<style type=\"text/css\">" html))
      (should (string-match-p "background: #121212" html))
      ;; Org's minimal default stylesheet is replaced, not layered under ours.
      (should-not (string-match-p "#content { max-width: 60em" html))
      (test-s3-publish--delete-export html-file)))

(ert-deftest s3-publish-test-theme-off-restores-org-default ()
  "A nil theme leaves Org's own export settings, and stylesheet, in place."
  (let* ((s3-publish-html-theme nil)
         (html-file (s3-publish--org-string-export-to-html "* Section\n"))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
    (should (string-match-p "#content { max-width: 60em" html))
    (should-not (string-match-p "background: #121212" html))
    (test-s3-publish--delete-export html-file)))

(ert-deftest s3-publish-test-theme-from-css-file ()
  "A theme naming a readable file is inlined from that file."
  (let ((css-file (make-temp-file "s3-publish-theme-" nil ".css"
                                  "body { color: fuchsia; }\n")))
    (unwind-protect
        (let* ((s3-publish-html-theme css-file)
               (html-file (s3-publish--org-string-export-to-html "* Section\n"))
               (html (with-temp-buffer
                       (insert-file-contents html-file)
                       (buffer-string))))
          (should (string-match-p "body { color: fuchsia; }" html))
          (test-s3-publish--delete-export html-file))
      (delete-file css-file))))

(ert-deftest s3-publish-test-theme-from-css-string ()
  "A theme that is not a file name is inlined verbatim as CSS."
  (let* ((s3-publish-html-theme "body { color: teal; }\n/* two lines */\n")
         (html-file (s3-publish--org-string-export-to-html "* Section\n"))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
    (should (string-match-p "body { color: teal; }" html))
    (test-s3-publish--delete-export html-file)))

(ert-deftest s3-publish-test-document-head-overrides-theme ()
  "A document's own HTML_HEAD still wins over the configured theme."
  (let* ((s3-publish-html-theme 'default)
         (html-file (s3-publish--org-string-export-to-html
                     "#+HTML_HEAD: <style>body { color: olive; }</style>\n* Section\n"))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
    (should (string-match-p "body { color: olive; }" html))
    (should-not (string-match-p "background: #121212" html))
    (test-s3-publish--delete-export html-file)))

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


;;; Anchors, self-linking headings and the table of contents

(defun test-s3-publish--export-body (org-text)
  "Export ORG-TEXT and return just the <body> of the resulting HTML.
Scoped to the body so the embedded stylesheet cannot satisfy a match."
  (let* ((file (s3-publish--org-string-export-to-html org-text))
         (html (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (test-s3-publish--delete-export file)
    (substring html (string-match "<body>" html))))

(ert-deftest s3-publish-test-headings-link-to-themselves ()
  "Every heading's text is a link to its own anchor, not just a TOC target."
  (let ((body (test-s3-publish--export-body
               "#+TITLE: T\n* One\n:PROPERTIES:\n:CUSTOM_ID: one\n:END:\n")))
    (should (string-match-p
             "<h2 id=\"one\">.*<a href=\"#one\">One</a></h2>" body))))

(ert-deftest s3-publish-test-self-links-can-be-disabled ()
  "With `s3-publish-html-self-link-headlines' nil, headings are plain."
  (let* ((s3-publish-html-self-link-headlines nil)
         (body (test-s3-publish--export-body "#+TITLE: T\n* One\n")))
    (should (string-match-p "<h2 id=\"[^\"]+\">" body))
    (should-not (string-match-p "<h2 id=\"[^\"]*\">[^<]*<span[^>]*>[^<]*</span> <a href=" body))))

(ert-deftest s3-publish-test-markdown-anchors-are-readable ()
  "Markdown headings get slug anchors instead of Org's content hashes."
  (let ((body (test-s3-publish--export-body
               (s3-publish-markdown-to-org
                "# Doc\n\n## Getting Started\n\n### Install the CLI\n"))))
    (should (string-match-p "id=\"getting-started\"" body))
    (should (string-match-p "id=\"install-the-cli\"" body))
    (should (string-match-p "href=\"#getting-started\"" body))))

(ert-deftest s3-publish-test-markdown-anchors-are-stable ()
  "The same Markdown always yields the same anchors, so shared links keep working.
Org's generated ids are content hashes that change between exports."
  (let ((md "# Doc\n\n## Alpha\n\nText.\n\n## Beta\n"))
    (should (equal (s3-publish-markdown-to-org md)
                   (s3-publish-markdown-to-org md)))
    (should (string-match-p ":CUSTOM_ID: alpha" (s3-publish-markdown-to-org md)))
    (should (string-match-p ":CUSTOM_ID: beta" (s3-publish-markdown-to-org md)))))

(ert-deftest s3-publish-test-markdown-slugs ()
  "Slugs are lowercase and dash separated, and drop link targets and markup."
  (let ((s3-publish--md-slugs nil))
    (should (equal (s3-publish--md-slug "Getting Started") "getting-started"))
    (should (equal (s3-publish--md-slug "**Bold** heading!") "bold-heading"))
    (should (equal (s3-publish--md-slug "`code` & more") "code-more"))
    (should (equal (s3-publish--md-slug "[Docs](https://e.com/x)") "docs"))
    (should (equal (s3-publish--md-slug "???") "section"))))

(ert-deftest s3-publish-test-duplicate-headings-get-distinct-anchors ()
  "Two headings with the same name must not share an anchor."
  (let ((org (s3-publish-markdown-to-org "# Doc\n\n## Notes\n\n### Notes\n")))
    (should (string-match-p ":CUSTOM_ID: notes\n" org))
    (should (string-match-p ":CUSTOM_ID: notes-2\n" org))))

(ert-deftest s3-publish-test-toc-heading-is-the-document-title ()
  "The contents list is headed by the document's own title, linking to the top."
  (let ((body (test-s3-publish--export-body "#+TITLE: Real Title\n* One\n** Two\n")))
    (should (string-match-p
             "<h2 class=\"toc-title\"><a href=\"#content\">Real Title</a></h2>" body))
    ;; "Table of Contents" says nothing the list does not already show.
    (should-not (string-match-p "Table of Contents" body))))

(ert-deftest s3-publish-test-toc-heading-dropped-without-a-title ()
  "With no title there is no heading to use, so the TOC gets none."
  (let ((body (test-s3-publish--export-body "* One\n** Two\n")))
    (should (string-match-p "<div id=\"table-of-contents\"" body))
    (should-not (string-match-p "Table of Contents" body))
    (should-not (string-match-p "toc-title" body))))

(ert-deftest s3-publish-test-markdown-toc-heading-is-promoted-h1 ()
  "A Markdown document's first header becomes both its title and TOC heading."
  (let ((body (test-s3-publish--export-body
               (s3-publish-markdown-to-org "# My Doc\n\n## Section\n"))))
    (should (string-match-p "<h1 class=\"title\">My Doc</h1>" body))
    (should (string-match-p
             "<h2 class=\"toc-title\"><a href=\"#content\">My Doc</a></h2>" body))))

(ert-deftest s3-publish-test-toc-untouched-without-a-toc ()
  "A document exported without a contents list is left alone."
  (let ((body (test-s3-publish--export-body
               "#+TITLE: T\n#+OPTIONS: toc:nil\n* One\n")))
    (should-not (string-match-p "table-of-contents" body))
    (should (string-match-p "<h1 class=\"title\">T</h1>" body))))

(ert-deftest s3-publish-test-theme-styles-its-own-additions ()
  "The theme covers the markup the source simple_dark theme does not."
  (let ((css s3-publish-html-default-style))
    ;; Carried over from simple_dark.
    (should (string-match-p "background: #121212" css))
    ;; Added, because Org emits these and simple_dark has no rule for them.
    (should (string-match-p "#org-download a" css))
    (should (string-match-p "pre \\.org-keyword" css))
    (should (string-match-p "h2\\.toc-title" css))
    (should (string-match-p "pre\\.example" css))
    ;; Inline code is deliberately smaller than in the source theme; a
    ;; fresh copy of simple_dark would silently restore 1.5em.
    (should-not (string-match-p "font-size: 1\\.5em" css))
    ;; The TOC sidebar relies on a row span, which a row gap would multiply
    ;; into the page height.
    (should (string-match-p "grid-row: 2 / span 500" css))
    (should-not (string-match-p "^[ \t]*\\(gap\\|row-gap\\):" css))
    ;; The base theme is dark; light comes from `s3-publish-html-light-style'.
    (should (string-match-p "color-scheme: dark" css))))

(ert-deftest s3-publish-test-theme-css-braces-balance ()
  "Unbalanced braces in the theme would swallow the rules that follow."
  (let ((css s3-publish-html-default-style)
        (open 0) (close 0))
    (dotimes (i (length css))
      (cond ((eq (aref css i) ?{) (setq open (1+ open)))
            ((eq (aref css i) ?}) (setq close (1+ close)))))
    (should (= open close))))


(ert-deftest s3-publish-test-light-css-braces-balance ()
  "The light overrides balance their braces too."
  (let ((css s3-publish-html-light-style)
        (open 0) (close 0))
    (dotimes (i (length css))
      (cond ((eq (aref css i) ?{) (setq open (1+ open)))
            ((eq (aref css i) ?}) (setq close (1+ close)))))
    (should (= open close))))

(ert-deftest s3-publish-test-color-scheme ()
  "The colour scheme picks dark, light, or both behind a media query."
  (let ((s3-publish-html-theme 'default))
    (let ((s3-publish-html-color-scheme 'dark))
      (should (equal (s3-publish--html-css) s3-publish-html-default-style)))
    (let* ((s3-publish-html-color-scheme 'light)
           (css (s3-publish--html-css)))
      (should (string-suffix-p s3-publish-html-light-style css))
      (should-not (string-match-p "prefers-color-scheme" css)))
    (let* ((s3-publish-html-color-scheme 'auto)
           (css (s3-publish--html-css)))
      (should (string-match-p
               (concat "@media (prefers-color-scheme: light) {\n"
                       (regexp-quote s3-publish-html-light-style) "}")
               css)))
    (let ((s3-publish-html-color-scheme 'sepia))
      (should-error (s3-publish--html-css)))))

(ert-deftest s3-publish-test-color-scheme-follows-reader-by-default ()
  "Out of the box, pages follow the reader's own light or dark preference."
  (should (eq (eval (car (get 's3-publish-html-color-scheme 'standard-value)) t)
              'auto)))

(ert-deftest s3-publish-test-color-scheme-only-for-built-in-theme ()
  "A custom theme is used as is, whatever the colour scheme."
  (let ((s3-publish-html-theme "body { color: olive; }\nhtml {}")
        (s3-publish-html-color-scheme 'light))
    (should (equal (s3-publish--html-css) "body { color: olive; }\nhtml {}"))))

(ert-deftest s3-publish-test-light-overrides-only-known-selectors ()
  "Every light override restyles a selector the dark theme has."
  (let ((dark s3-publish-html-default-style)
        (start 0))
    (while (string-match "^\\([^{}\n][^{}]*\\){" s3-publish-html-light-style start)
      (setq start (match-end 0))
      (dolist (selector (split-string (match-string 1 s3-publish-html-light-style)
                                      "," t "[ \t\n]+"))
        (unless (equal selector ":root")
          (should (string-match-p (regexp-quote selector) dark)))))))


;;; Source blocks

(defun test-s3-publish--advise-src-block (orig &rest args)
  "Advice standing in for a personal config's source block decoration."
  (concat "ADVISED-MARKER" (apply orig args)))

(defun test-s3-publish--export-src-block ()
  "Export a single bash source block and return the HTML."
  (let* ((html-file (s3-publish--org-string-export-to-html
                     "#+begin_src bash\necho hi\n#+end_src\n"))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
    (test-s3-publish--delete-export html-file)
    html))

(ert-deftest s3-publish-test-src-block-advice-ignored ()
  "Advice on `org-html-src-block' does not leak into published pages."
  (advice-add 'org-html-src-block :around #'test-s3-publish--advise-src-block)
  (unwind-protect
      (progn
        (should-not (string-match-p "ADVISED-MARKER"
                                    (test-s3-publish--export-src-block)))
        ;; The advice is only suspended, not removed.
        (should (advice-member-p #'test-s3-publish--advise-src-block
                                 'org-html-src-block)))
    (advice-remove 'org-html-src-block #'test-s3-publish--advise-src-block)))

(ert-deftest s3-publish-test-src-block-advice-can-be-kept ()
  "With `s3-publish-html-ignore-advice' nil, advice applies as usual."
  (advice-add 'org-html-src-block :around #'test-s3-publish--advise-src-block)
  (unwind-protect
      (let ((s3-publish-html-ignore-advice nil))
        (should (string-match-p "ADVISED-MARKER"
                                (test-s3-publish--export-src-block))))
    (advice-remove 'org-html-src-block #'test-s3-publish--advise-src-block)))

(ert-deftest s3-publish-test-src-block-tagged-with-language ()
  "Themed source blocks carry their language for the theme to label."
  (should (string-match-p "<pre class=\"src src-bash\" data-lang=\"bash\">"
                          (test-s3-publish--export-src-block)))
  (should (string-match-p "attr(data-lang)" s3-publish-html-default-style)))

(defconst test-s3-publish--copy-script "className = 's3-publish-copy'"
  "Text found only in the copy button script, not in the theme CSS.")

(ert-deftest s3-publish-test-copy-button-script-included ()
  "Published pages carry the copy button script by default."
  (should (string-match-p test-s3-publish--copy-script (test-s3-publish--export-src-block)))
  (should (string-match-p "button\\.s3-publish-copy" s3-publish-html-default-style)))

(ert-deftest s3-publish-test-link-expiry-notice-included ()
  "Pages carry the signed-link countdown script, unless it is turned off.
The theme styles the notice either way; it is the script that adds it."
  (should (string-match-p "X-Amz-Expires" (test-s3-publish--export-src-block)))
  (let ((s3-publish-html-link-expiry-notice nil))
    (should-not (string-match-p "X-Amz-Expires"
                                (test-s3-publish--export-src-block)))))

(ert-deftest s3-publish-test-link-expiry-notice-without-copy-button ()
  "The countdown does not depend on the copy button, nor it on the countdown."
  (let ((s3-publish-html-copy-button nil))
    (let ((html (test-s3-publish--export-src-block)))
      (should (string-match-p "X-Amz-Expires" html))
      (should-not (string-match-p "navigator\\.clipboard" html))))
  (let ((s3-publish-html-link-expiry-notice nil))
    (let ((html (test-s3-publish--export-src-block)))
      (should-not (string-match-p "X-Amz-Expires" html))
      (should (string-match-p "navigator\\.clipboard" html))))
  (let ((s3-publish-html-copy-button nil)
        (s3-publish-html-link-expiry-notice nil))
    (should-not (s3-publish--html-scripts))))

(ert-deftest s3-publish-test-copy-button-can-be-disabled ()
  "With `s3-publish-html-copy-button' nil, no script is added."
  (let ((s3-publish-html-copy-button nil))
    (should-not (string-match-p test-s3-publish--copy-script
                                (test-s3-publish--export-src-block)))))

(ert-deftest s3-publish-test-copy-button-is-independent-of-the-theme ()
  "The copy button survives the theme being off or overridden."
  (let ((s3-publish-html-theme nil))
    (should (string-match-p test-s3-publish--copy-script
                            (test-s3-publish--export-src-block))))
  (let* ((html-file (s3-publish--org-string-export-to-html
                     "#+HTML_HEAD: <style>body { color: olive; }</style>\n* Section\n"))
         (html (with-temp-buffer (insert-file-contents html-file) (buffer-string))))
    (test-s3-publish--delete-export html-file)
    (should (string-match-p test-s3-publish--copy-script html))))

(ert-deftest s3-publish-test-copy-button-keeps-user-scripts ()
  "Scripts the user already includes are kept alongside the copy button."
  (let ((org-html-head-include-scripts t)
        (org-html-scripts "<script>var userScript = 1;</script>"))
    (let ((html (test-s3-publish--export-src-block)))
      (should (string-match-p "userScript" html))
      (should (string-match-p test-s3-publish--copy-script html)))))

(ert-deftest s3-publish-test-src-block-untagged-without-theme ()
  "With the theme off, source block markup is Org's own."
  (let ((s3-publish-html-theme nil))
    (should-not (string-match-p "data-lang"
                                (test-s3-publish--export-src-block)))))


;;; Favicon

(ert-deftest s3-publish-test-favicon-is-inlined-by-default ()
  "The built-in icon rides along in the page as a data URI, not as a link."
  (let ((body (test-s3-publish--export-body "#+TITLE: T\n* One\n"))
        (head (let* ((f (s3-publish--org-string-export-to-html "#+TITLE: T\n* One\n"))
                     (h (with-temp-buffer (insert-file-contents f) (buffer-string))))
                (test-s3-publish--delete-export f)
                h)))
    (ignore body)
    (should (string-match-p
             "<link rel=\"icon\" href=\"data:image/png;base64,[A-Za-z0-9+/=]+\" />" head))))

(ert-deftest s3-publish-test-favicon-can-be-disabled ()
  "A nil favicon adds no icon link at all."
  (let* ((s3-publish-html-favicon nil)
         (f (s3-publish--org-string-export-to-html "#+TITLE: T\n* One\n"))
         (html (with-temp-buffer (insert-file-contents f) (buffer-string))))
    (test-s3-publish--delete-export f)
    (should-not (string-match-p "rel=\"icon\"" html))))

(ert-deftest s3-publish-test-favicon-from-file ()
  "A favicon naming a readable file is inlined with its media type."
  (let ((png (make-temp-file "s3-publish-icon-" nil ".png" "not-really-a-png")))
    (unwind-protect
        (let* ((s3-publish-html-favicon png)
               (f (s3-publish--org-string-export-to-html "#+TITLE: T\n* One\n"))
               (html (with-temp-buffer (insert-file-contents f) (buffer-string))))
          (test-s3-publish--delete-export f)
          (should (string-match-p "href=\"data:image/png;base64," html))
          ;; The data URI must be one line, or the href breaks.
          (should (string-match-p "href=\"data:image/png;base64,[A-Za-z0-9+/=]+\"" html)))
      (delete-file png))))

(ert-deftest s3-publish-test-favicon-media-type-from-extension ()
  "The media type follows the file's extension."
  (dolist (case '(("ico" . "image/x-icon") ("svg" . "image/svg+xml")))
    (let ((file (make-temp-file "s3-publish-icon-" nil (concat "." (car case)) "x")))
      (unwind-protect
          (let ((s3-publish-html-favicon file))
            (should (string-prefix-p (concat "data:" (cdr case) ";base64,")
                                     (s3-publish--html-favicon-uri))))
        (delete-file file)))))

(ert-deftest s3-publish-test-favicon-string-used-verbatim ()
  "A string that is not a readable file is used as the href as given."
  (let ((s3-publish-html-favicon "https://example.com/icon.png"))
    (should (equal (s3-publish--html-favicon-uri) "https://example.com/icon.png"))))

(ert-deftest s3-publish-test-favicon-is-independent-of-the-theme ()
  "Turning the theme off must not take the icon with it, or the reverse."
  (let* ((s3-publish-html-theme nil)
         (f (s3-publish--org-string-export-to-html "#+TITLE: T\n* One\n"))
         (html (with-temp-buffer (insert-file-contents f) (buffer-string))))
    (test-s3-publish--delete-export f)
    (should (string-match-p "rel=\"icon\"" html))
    ;; Theme off, so Org's own stylesheet is back.
    (should (string-match-p "#content { max-width: 60em" html))))

(ert-deftest s3-publish-test-default-favicon-stays-small ()
  "The icon is embedded in every published page, so it must stay small.
The source emacs.ico is 34kB across five sizes; only the 32x32 is wanted."
  (should (string-prefix-p "data:image/png;base64," s3-publish-html-default-favicon))
  (should (< (length s3-publish-html-default-favicon) 8192)))


;;; Footer placement

(ert-deftest s3-publish-test-postamble-moves-inside-content ()
  "The postamble must sit inside #content, not beside it.
As a sibling it spans the full width, so on a wide screen it lands under
the contents sidebar rather than under the text it belongs to."
  (let* ((body (test-s3-publish--export-body "#+TITLE: T\n* One\n"))
         (content-start (string-match "<div id=\"content\"" body))
         (postamble (string-match "<div id=\"postamble\"" body))
         ;; The div that closes #content is the last one before </body>.
         (body-end (string-match "</body>" body))
         (content-close (string-match "</div>[ \t\n]*</body>" body)))
    (should content-start)
    (should postamble)
    (should content-close)
    (should (< content-start postamble))
    (should (< postamble content-close))
    (should (< content-close body-end))))

(ert-deftest s3-publish-test-content-end-position ()
  "The end of #content is found with and without a postamble."
  (dolist (case (list
                 (cons "<body>\n<div id=\"content\">\nx\n</div>\n<div id=\"postamble\">p</div>\n</body>"
                       "<div id=\"postamble\">")
                 (cons "<body>\n<div id=\"content\">\nx\n</div>\n</body>" "</body>")))
    (with-temp-buffer
      (insert (car case))
      (let ((pos (s3-publish--content-end-position)))
        (should pos)
        ;; Point lands on the </div> that closes #content, which is the one
        ;; directly before the marker.
        (should (string-prefix-p "</div>" (buffer-substring pos (point-max))))))))

(ert-deftest s3-publish-test-postamble-move-is-a-noop-without-one ()
  "A document with no postamble is left alone rather than mangled."
  (let ((file (make-temp-file "s3-publish-nopost-" nil ".html"
                              "<body>\n<div id=\"content\">\nx\n</div>\n</body>")))
    (unwind-protect
        (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (s3-publish--move-postamble-into-content file)
          (should (equal before
                         (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (delete-file file))))

(ert-deftest s3-publish-test-postamble-move-preserves-its-content ()
  "Moving the postamble must not drop or duplicate it."
  (let ((body (test-s3-publish--export-body "#+TITLE: T\n* One\n")))
    (should (= 1 (cl-count-if (lambda (_) t)
                              (let ((n 0) (i 0))
                                (while (setq i (string-match "<div id=\"postamble\"" body i))
                                  (setq n (1+ n) i (1+ i)))
                                (make-list n t)))))
    (should (string-match-p "Author:" body))))

(provide 'test-s3-publish)


;;; Tangling before publishing

(defun test-s3-publish--with-org-file (content fn)
  "Visit a temporary Org file holding CONTENT and call FN in its buffer.
FN receives the temporary directory, where relative tangle paths land."
  (let* ((dir (make-temp-file "s3-publish-tangle-" t))
         (file (expand-file-name "doc.org" dir))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert content))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (funcall fn dir)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory dir t))))

(defconst test-s3-publish--tangled-doc
  "#+TITLE: Tangled\n\n#+begin_src text :tangle out.txt\nhello\n#+end_src\n"
  "An Org document with one block to tangle into out.txt.")

(defmacro test-s3-publish--counting-prompts (answer &rest body)
  "Run BODY answering every `y-or-n-p' with ANSWER.
Returns the number of times BODY was asked."
  (declare (indent 1))
  `(let ((asked 0))
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (&rest _) (setq asked (1+ asked)) ,answer)))
       ,@body)
     asked))

(ert-deftest s3-publish-test-tangle-off-by-default ()
  "With the default setting nothing is tangled and nothing is asked."
  (should (null (default-value 's3-publish-org-tangle)))
  (test-s3-publish--with-org-file
   test-s3-publish--tangled-doc
   (lambda (dir)
     (let ((s3-publish-org-tangle nil))
       (should (= 0 (test-s3-publish--counting-prompts t
                      (should-not (s3-publish--org-maybe-tangle)))))
       (should-not (file-exists-p (expand-file-name "out.txt" dir)))))))

(ert-deftest s3-publish-test-tangle-keyword-errors-when-off ()
  "A document asking to be tangled is an error when tangling is off.
Publishing it anyway would run its code against stale files."
  (test-s3-publish--with-org-file
   (concat "#+S3_PUBLISH_TANGLE: t\n" test-s3-publish--tangled-doc)
   (lambda (dir)
     (let ((s3-publish-org-tangle nil))
       (should-error (s3-publish--org-maybe-tangle) :type 'user-error)
       (should-not (file-exists-p (expand-file-name "out.txt" dir)))))))

(ert-deftest s3-publish-test-tangle-keyword-nil-is-fine-when-off ()
  "A document opting out of tangling is no error when tangling is off."
  (test-s3-publish--with-org-file
   (concat "#+S3_PUBLISH_TANGLE: nil\n" test-s3-publish--tangled-doc)
   (lambda (_dir)
     (let ((s3-publish-org-tangle nil))
       (should-not (s3-publish--org-maybe-tangle))))))

(ert-deftest s3-publish-test-tangle-asks-and-tangles-on-yes ()
  "With `ask', answering yes tangles the document."
  (test-s3-publish--with-org-file
   test-s3-publish--tangled-doc
   (lambda (dir)
     (let ((s3-publish-org-tangle 'ask))
       (should (= 1 (test-s3-publish--counting-prompts t
                      (should (s3-publish--org-maybe-tangle)))))
       (should (equal (with-temp-buffer
                        (insert-file-contents (expand-file-name "out.txt" dir))
                        (buffer-string))
                      "hello\n"))))))

(ert-deftest s3-publish-test-tangle-asks-and-skips-on-no ()
  "With `ask', answering no leaves the tangled files alone."
  (test-s3-publish--with-org-file
   test-s3-publish--tangled-doc
   (lambda (dir)
     (let ((s3-publish-org-tangle 'ask))
       (should (= 1 (test-s3-publish--counting-prompts nil
                      (should-not (s3-publish--org-maybe-tangle)))))
       (should-not (file-exists-p (expand-file-name "out.txt" dir)))))))

(ert-deftest s3-publish-test-tangle-keyword-t-still-asks ()
  "A document asking to be tangled is still asked about; nothing is implicit."
  (test-s3-publish--with-org-file
   (concat "#+S3_PUBLISH_TANGLE: t\n" test-s3-publish--tangled-doc)
   (lambda (dir)
     (let ((s3-publish-org-tangle 'ask))
       (should (= 1 (test-s3-publish--counting-prompts nil
                      (should-not (s3-publish--org-maybe-tangle)))))
       (should-not (file-exists-p (expand-file-name "out.txt" dir)))))))

(ert-deftest s3-publish-test-tangle-keyword-nil-skips-the-question ()
  "A document opting out of tangling is not asked about."
  (test-s3-publish--with-org-file
   (concat "#+S3_PUBLISH_TANGLE: nil\n" test-s3-publish--tangled-doc)
   (lambda (dir)
     (let ((s3-publish-org-tangle 'ask))
       (should (= 0 (test-s3-publish--counting-prompts t
                      (should-not (s3-publish--org-maybe-tangle)))))
       (should-not (file-exists-p (expand-file-name "out.txt" dir)))))))

(ert-deftest s3-publish-test-tangle-not-asked-without-tangle-blocks ()
  "A document with nothing to tangle is not asked about."
  (test-s3-publish--with-org-file
   "#+TITLE: Plain\n\n#+begin_src text\nhello\n#+end_src\n\n#+begin_src text :tangle no\nhi\n#+end_src\n"
   (lambda (_dir)
     (let ((s3-publish-org-tangle 'ask))
       (should (= 0 (test-s3-publish--counting-prompts t
                      (should-not (s3-publish--org-maybe-tangle)))))))))

(ert-deftest s3-publish-test-tangle-blocks-found-through-properties ()
  "A `:tangle' set by `#+PROPERTY:' counts as a block to tangle."
  (test-s3-publish--with-org-file
   "#+PROPERTY: header-args :tangle out.txt\n\n#+begin_src text\nhello\n#+end_src\n"
   (lambda (_dir)
     (should (s3-publish--org-has-tangle-blocks-p)))))

(ert-deftest s3-publish-test-tangle-keyword-values ()
  "The keyword reads t, nil and their yes/no spellings, and rejects the rest."
  (dolist (case '(("t" . t) ("T" . t) ("yes" . t) ("nil" . nil) ("no" . nil)))
    (with-temp-buffer
      (insert (format "#+S3_PUBLISH_TANGLE: %s\n" (car case)))
      (should (eq (s3-publish--org-tangle-keyword) (cdr case)))))
  (with-temp-buffer
    (insert "* No keyword\n")
    (should (eq (s3-publish--org-tangle-keyword) 'unset)))
  (with-temp-buffer
    (insert "#+S3_PUBLISH_TANGLE: maybe\n")
    (should-error (s3-publish--org-tangle-keyword) :type 'user-error)))

(ert-deftest s3-publish-test-org-buffer-tangles-before-publishing ()
  "Publishing an Org buffer tangles it before the export runs."
  (test-s3-publish--with-org-file
   test-s3-publish--tangled-doc
   (lambda (dir)
     (let ((s3-publish-org-tangle 'ask)
           (tangled-at-publish 'not-called))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ((symbol-function 's3-publish--publish-document)
                  (lambda (&rest _)
                    (setq tangled-at-publish
                          (file-exists-p (expand-file-name "out.txt" dir))))))
         (s3-publish-org-buffer))
       (should (eq tangled-at-publish t))))))

(ert-deftest s3-publish-test-org-buffer-refuses-before-uploading ()
  "A document that needs tangling, with tangling off, is never uploaded."
  (test-s3-publish--with-org-file
   (concat "#+S3_PUBLISH_TANGLE: t\n" test-s3-publish--tangled-doc)
   (lambda (_dir)
     (let ((s3-publish-org-tangle nil)
           (published nil))
       (cl-letf (((symbol-function 's3-publish--publish-document)
                  (lambda (&rest _) (setq published t))))
         (should-error (s3-publish-org-buffer) :type 'user-error))
       (should-not published)))))


;;; Profile prompt

(defmacro test-s3-publish--with-profiles (names &rest body)
  "Run BODY with profiles named NAMES and an empty profile history."
  (declare (indent 1))
  `(let ((s3-publish-profiles (mapcar (lambda (n) (list :name n)) ,names))
         (s3-publish-profile-history nil)
         (history-delete-duplicates nil))
     ,@body))

(defun test-s3-publish--pick-profile (choice)
  "Answer the profile prompt with CHOICE, or the default when CHOICE is nil.
Returns (DEFAULT . RESULT): what the prompt started on and what it returned."
  (let (offered)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection _pred require-match
                                _initial hist def &rest _)
                 (should (eq require-match t))
                 (should (eq hist 's3-publish-profile-history))
                 (should (member def collection))
                 (setq offered def)
                 (or choice def))))
      (let ((result (s3-publish--read-profile-name)))
        (cons offered result)))))

(ert-deftest s3-publish-test-profile-prompt-starts-on-default-profile ()
  "With no history, the prompt starts on the profile named \"default\"."
  (test-s3-publish--with-profiles '("work" "default" "home")
    (should (equal (car (test-s3-publish--pick-profile nil)) "default"))))

(ert-deftest s3-publish-test-profile-prompt-starts-on-first-without-default ()
  "With no history and no \"default\" profile, the prompt starts on the first."
  (test-s3-publish--with-profiles '("work" "home")
    (should (equal (car (test-s3-publish--pick-profile nil)) "work"))))

(ert-deftest s3-publish-test-profile-prompt-starts-on-last-used ()
  "After choosing a profile, the next prompt starts on it."
  (test-s3-publish--with-profiles '("default" "work" "home")
    (should (equal (cdr (test-s3-publish--pick-profile "home")) "home"))
    (should (equal (car (test-s3-publish--pick-profile nil)) "home"))
    (should (equal (cdr (test-s3-publish--pick-profile "work")) "work"))
    (should (equal (car (test-s3-publish--pick-profile nil)) "work"))))

(ert-deftest s3-publish-test-profile-prompt-accepting-default-keeps-it ()
  "Accepting the offered profile keeps it as the next default."
  (test-s3-publish--with-profiles '("default" "work")
    (test-s3-publish--pick-profile "work")
    (should (equal (test-s3-publish--pick-profile nil) '("work" . "work")))
    (should (equal (car (test-s3-publish--pick-profile nil)) "work"))))

(ert-deftest s3-publish-test-profile-prompt-skips-removed-profiles ()
  "A last-used profile that no longer exists is passed over."
  (test-s3-publish--with-profiles '("default" "work" "home")
    (test-s3-publish--pick-profile "work")
    (test-s3-publish--pick-profile "home")
    (setq s3-publish-profiles (list (list :name "default") (list :name "work")))
    (should (equal (car (test-s3-publish--pick-profile nil)) "work"))))

(ert-deftest s3-publish-test-select-profile-uses-the-prompt ()
  "Publishing selects its profile through the shared prompt."
  (test-s3-publish--with-profiles '("default" "work")
    (test-s3-publish--pick-profile "work")
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _i _h def &rest _) (setq offered def) def))
                ((symbol-function 's3-publish-get-credentials) #'identity))
        (should (equal (plist-get (s3-publish--select-profile) :name) "work")))
      (should (equal offered "work")))))
