(depends-on "emacs")
(depends-on "ert") ; For testing
(depends-on "some-dependency") ; Replace with your package dependencies

(task test
  "Run package tests using ert."
  (shell "emacs --batch -l ert -l tests/test-helper.el -l tests/test-your-package.el -f ert-run-tests-batch-and-exit"))
