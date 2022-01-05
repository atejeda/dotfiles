#!/usr/bin/env bash

set -o nounset
set -o errexit

CURRENT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# vim setup

VIMROOT=$CURRENT/.vim

mkdir -p $VIMROOT/bundle
git clone https://github.com/VundleVim/Vundle.vim.git $VIMROOT/bundle/Vundle.vim

mkdir -p $VIMROOT/colors

TMPDIR=/tmp/$(uuidgen)
git clone https://github.com/flazz/vim-colorschemes.git $TMPDIR
rsync -avpq $TMPDIR/colors/ $VIMROOT/colors/
rm -rf $TMPDIR

TMPDIR=/tmp/$(uuidgen)
git clone https://github.com/gosukiwi/vim-atom-dark $TMPDIR
rsync -avpq $TMPDIR/colors/ $VIMROOT/colors/
rm -rf $TMPDIR

# stow

stow .

echo ". $HOME/.profile" >> $HOME/.bashrc

echo "...done..."
