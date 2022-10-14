export MY_EMACS_TOOLNAME=$(basename "$0")
export MY_EMACS_DOT_PATH=$(readlink -f "$(dirname $0)/..")
export MY_EMACS_DATA_PATH=${MY_EMACS_DOT_PATH}/data
export MY_EMACS_EL_PATH=${MY_EMACS_DOT_PATH}/my
export MY_EMACS_PRIVATE_EL_PATH=${MY_EMACS_DOT_PATH}/my_private
export MY_EMACS_SITE_EL_PATH=${MY_EMACS_DOT_PATH}/site
export MY_EMSCS_TOOLS_PATH=${MY_EMACS_DOT_PATH}/tools
