((org-mode
  (eval . (add-hook 'after-save-hook
                    (lambda ()
                      (when (string= (file-name-extension buffer-file-name) "org")
                        (org-babel-tangle)))
                    nil t))))
