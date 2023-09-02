MK_EMACS_OUTPUT=mk-emacs.el
GEN_NIX_PACKAGE=./tasks/gen-nix-package.sh

all: lint $(MK_EMACS_OUTPUT)

lint:
	./tasks/lint.sh

$(MK_EMACS_OUTPUT): $(GEN_NIX_PACKAGE) my.features/*.el my.lang-modes/*.el
	$(GEN_NIX_PACKAGE) "$@"
	# Hey, don't question it, it's my own configuration, okay?
	cp "$@" ../hw.nixpkgs/applications/mk-emacs.nix

.PHONY: all lint
