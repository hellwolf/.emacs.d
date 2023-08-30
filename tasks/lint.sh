#!/usr/bin/env bash

D="$(dirname "$0")"
ELISP_LINT=(emacs -Q --batch -l init.el -l elisp-lint-config.el -f elisp-lint-files-batch)

cleanup() {
    rm -f {libs,my.features,my.lang-modes}/*.elc
    rm -f {libs,my.features,my.lang-modes}/*-autoloads.el
}
trap cleanup EXIT

set -x

cd "$D"/..
cleanup
FAILED=0
"${ELISP_LINT[@]}" "$PWD"/libs/*.el || FAILED=1
"${ELISP_LINT[@]}" "$PWD"/my.features/*.el || FAILED=1
"${ELISP_LINT[@]}" "$PWD"/my.lang-modes/*.el || FAILED=1
exit "$FAILED"
