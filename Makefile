MK_EMACS_OUTPUT=../hw.nixpkgs/applications/mk-emacs.nix

all: lint $(MK_EMACS_OUTPUT)

lint:
	./tasks/lint.sh

$(MK_EMACS_OUTPUT): my.features/*.el my.lang-modes/*.el
	./tasks/gen-nix-package.sh "$@"

.PHONY: all lint
