#!/usr/bin/env bash

D="$(dirname "$0")"
OUTPUT_FILE=$1

set -e
cd "$D"/..
epkgs=$(grep 'use-package\s+(\w|-)+\)?$'  my.features/ my.lang-modes/ -REoh | \
        sort | uniq | \
        sed -Es 's/use-package\s+([^)]+)\)?$/\1/g')
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

