D=$(dirname "$(readlink -f $0)")

rm -f ~/.emacs.d

ln -s $D ~/.emacs.d

ls -l ~/.emacs.d
