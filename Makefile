MK_EMACS_OUTPUT=mk-emacs.el

all: lint $(MK_EMACS_OUTPUT)

lint:
	./tasks/lint.sh

$(MK_EMACS_OUTPUT): my.features/*.el my.lang-modes/*.el
	./tasks/gen-nix-package.sh "$@"
	# Hey, don't question it, it's my own configuration, okay?
	cp "$@" ../hw.nixpkgs/applications/mk-emacs.nix

.PHONY: all lint
