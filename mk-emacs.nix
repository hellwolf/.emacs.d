# AUTO-GENERATED! DO NOT DELETE!
# This is the emacs nix package builder based on the use-packages needed.
pkgs: emacsVariant: pkgs.buildEnv {
  name = "hw-emacs";
  paths = [((pkgs.emacsPackagesFor emacsVariant).emacsWithPackages (epkgs: with epkgs; [
all-the-icons
bm
browse-kill-ring
cmake-mode
company
diminish
editorconfig
elisp-lint
evil
evil-org
evil-terminal-cursor-changer
flycheck
fzf
gnuplot
haskell-mode
highlight-symbol
htmlize
js2-mode
ligature
lsp-grammarly
lsp-haskell
lsp-mode
magit
nix-mode
ob-typescript
org
org-bullets
ox-reveal
plantuml-mode
python
solidity-mode
tabbar
treemacs
treemacs-evil
typescript-mode
undo-tree
vterm
vterm-toggle
wgrep
which-key
which-key-posframe
yaml-mode
yul-mode
zenburn-theme
    ]))
  ];
}
