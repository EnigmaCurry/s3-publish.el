;; latin-words.el --- A Latin-English dictionary -*- lexical-binding: t; -*-
;;
;; Author: EnigmaCurry
;; URL: https://github.com/EnigmaCurry/latin-words
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: dictionary, latin
;; SPDX-License-Identifier: MIT AND CC-BY-SA-3.0
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a Latin to English Dictionary based upon the work "A Latin
;; Dictionary" (1879) by Charlton T. Lewis and Charles Short,
;; published by the Perseus Digital Library at Tufts University
;; CC-BY-SA 3.0.
;;
;; See LICENSE.txt

;;; Usage:
;;
;; ;; Get description string for Latin word "adbibo":
;; (latin-word-get-description "adbibo")
;;
;; ;; Get definition object for Latin word "adbibo":
;; (latin-word-get-definition "adbibo")
;;    
;; ;; Get a "random" deterministic Latin word based on numeric seed:
;; (latin-word-get-by-seed 1234567890)
;;
;; ;; Get the Latin word of the day:
;; (latin-word-of-the-day)

(defgroup latin-words nil
  "Settings for the latin-words package."
  :prefix "latin-words-"
  :group 'libraries)

(defcustom latin-words-directory
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the Latin dictionary files."
  :type 'directory
  :group 'latin-words)

(defun latin-word-of-the-day ()
  (latin-word-get-by-seed (string-to-number (substring (secure-hash 'sha256 (format-time-string "%Y%m%d")) 0 16) 16)))

(defun latin-word-get-by-seed (&optional seed)
  "Retrieve a deterministic Latin word by a given SEED.
If called interactively, prompt for the seed."
  (interactive (list (read-number "Enter seed: ")))
  (let* ((seed (or seed (error "Seed must be provided")))
         (default-directory latin-words-directory)
         (command "jq -r '.[] | .key' *.json | sort -u")
         (words
          (split-string (shell-command-to-string command) "\n" t))
         (num-words (length words)))
    (if (> num-words 0)
        (let* ((index (mod seed num-words))
               (word (nth index words)))
          (message "%s" word)
          word)
      (message "Word not found."))))

(defun latin-word-get-definition (&optional word)
  "Lookup WORD in the Latin dictionary in the latin-words-directory.
If called interactively, prompt for the WORD."
  (interactive (list (read-string "Enter word: ")))
  (let* ((word (or word (error "Word must be provided")))
         (first-letter (upcase (substring word 0 1)))
         (file-name
          (concat latin-words-directory "/" "ls_" first-letter ".json"))
         ;; Updated jq command to flatten "senses"
         (jq-command
          (format
           "jq -r --arg word '%s' '.[] | select(.key == $word) | .senses |= (if type == \"array\" then flatten else . end)' %s"
           word file-name))
         (result (shell-command-to-string jq-command)))
    (if (string-blank-p result)
        (progn
          (error "No dictionary entry found for word: %s" word)
          nil)
      (let ((v (json-parse-string result)))
        (message result)
        v)
      )))

(defun latin-word-get-description (&optional word)
  "Lookup WORD in the Latin dictionary and return a formatted description.
If called interactively, prompt for the WORD."
  (interactive (list (read-string "Enter word: ")))
  (let* ((word (or word (error "Word must be provided")))
         (entry (latin-word-get-definition word)))
    (when (null entry)
      (error "No dictionary entry found for word: %s" word))
    (let* ((word (or (gethash "key" entry) ""))
           (part-of-speech (let ((pos (gethash "part_of_speech" entry)))
                         (if (eq pos :null) "" pos)))
           (gender (or (gethash "gender" entry) ""))
           (senses (mapconcat 'identity 
                              (latin-word-flatten-vector 
                               (or (gethash "senses" entry) []))
                              "\n\n"))
           (title-orthography (or (gethash "title_orthography" entry) ""))
           (main-notes (or (gethash "main_notes" entry) ""))
           (description (with-temp-buffer
                          (let* ((start (point)))
                            (insert (upcase word))
                            (unless (string-blank-p title-orthography)          
                              (insert " ")
                              (insert title-orthography))
                            (unless (string-blank-p part-of-speech)
                              (insert (concat " (" part-of-speech 
                                              (unless (string-blank-p gender) 
                                                (concat " " gender)) 
                                              ")")))
                            (unless (string-blank-p main-notes)          
                              (insert " ")
                              (insert main-notes))
                            (unless (string-blank-p senses)
                              (newline)
                              (newline)
                              (insert senses))
                            (fill-region start (point))
                            (buffer-string)))))
      (message description)
      description)))

(defun latin-word-flatten-vector (vec)
  "Flatten VEC, which may contain sub-vectors."
  (apply #'vector
         (cl-mapcan (lambda (item)
                      (if (vectorp item)
                          (latin-word-flatten-vector item)
                        (list item)))
                    vec)))

(provide 'latin-words)
