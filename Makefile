MK_EMACS_OUTPUT=mk-emacs.nix
GEN_NIX_PACKAGE=./tasks/gen-mk-emacs.sh

all: lint $(MK_EMACS_OUTPUT) test

lint:
	./tasks/lint.sh

$(MK_EMACS_OUTPUT): $(GEN_NIX_PACKAGE) my.features/*.el my.lang-modes/*.el
	$(GEN_NIX_PACKAGE) "$@"
	# Hey, don't question it, it's my own configuration, okay?
	cp "$@" ../hw.nixpkgs/applications/mk-emacs.nix

test: $(MK_EMACS_OUTPUT)
	nix build --dry-run --impure --expr " \
	    let pkgs = import <nixpkgs> {}; \
	        mk-emacs = import ./"$(MK_EMACS_OUTPUT)" pkgs; \
	    in { \
	        emacs28-gtk3 = mk-emacs pkgs.emacs28-gtk3; \
	        emacs29-gtk3 = mk-emacs pkgs.emacs29-gtk3; \
	    } \
		"

.PHONY: all lint test
