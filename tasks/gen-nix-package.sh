#!/usr/bin/env bash

D="$(dirname "$0")"
OUTPUT_FILE=$1

set -e
cd "$D"/..
epkgs=$(grep 'use-package (\w|-)+'  my.features/ my.lang-modes/ -REoh | sort | uniq | sed 's/use-package //g')
{
    echo "# AUTO-GENERATED! DO NOT DELETE!"
    echo "# This is the emacs nix package builder based on the use-packages needed."
    cat <<EOF
pkgs: emacsVariant: pkgs.buildEnv {
  name = "hw-emacs";
  paths = [((pkgs.emacsPackagesFor emacsVariant).emacsWithPackages (epkgs: with epkgs; [
    $epkgs
    ]))
  ];
}
EOF
} | tee "$OUTPUT_FILE"

nix build --dry-run --impure --expr "
    let pkgs = import <nixpkgs> {};
    in import "$OUTPUT_FILE" pkgs pkgs.emacs29-gtk3
"
