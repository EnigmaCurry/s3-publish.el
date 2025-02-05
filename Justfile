set export

current_dir := `pwd`
RUST_LOG := "debug"

# print help for Just targets
help:
    @just -l

test:
    emacs --batch --debug-init -L . -L test -l test/all-tests.el -eval '(ert-run-tests-batch-and-exit)'

test-one TEST_NAME:
	eask emacs --batch -L . -L test -l test/all-tests.el -eval '(ert-run-tests-batch-and-exit "{{TEST_NAME}}")'
