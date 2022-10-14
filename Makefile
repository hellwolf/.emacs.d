ELISP_LINT_CONF = emacs -Q --batch -l init.el -l elisp-lint-configs.el -f elisp-lint-files-batch
ELISP_LINT_LIB = emacs -Q --batch -l init.el -l elisp-lint-libs.el -f elisp-lint-files-batch

lint: lint-configs lint-libs

lint-configs:
	$(ELISP_LINT_CONF) $(PWD)/init.el $(PWD)/my.features/*.el $(PWD)/my.lang-modes/*.el

lint-libs:
	$(ELISP_LINT_LIB) $(PWD)/libs/*.el

.PHONY: lint*
