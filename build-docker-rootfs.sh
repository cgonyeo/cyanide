#!/usr/bin/env bash

set -e

if [ -e out ]; then
    rm -rf out
fi

if [ -e out-* ]; then
    rm -rf out-*
fi

if [ -e docker-rootfs ]; then
    rm -rf docker-rootfs
fi

nix-env -i cyanide -f . -r -p out
nix-env -i vim -p out

mkdir -p docker-rootfs

for i in $(nix-store -qR out/); do
    mkdir -p $(dirname docker-rootfs/$i)
    cp -r $i docker-rootfs/$i
done
chmod -R +w docker-rootfs

mkdir -p docker-rootfs/root/.config/cyanide/
mkdir -p docker-rootfs/etc
mkdir -p docker-rootfs/bin

cat > docker-rootfs/root/.config/cyanide/cyanide.conf <<EOF
[DATABASE]
host = localhost
port = 5432
user = cyanide
password = cyanide
database = cyanide

[EDITOR]
editor = 
EOF

cat > docker-rootfs/root/.vimrc <<EOF
" Be iMproved
set nocompatible

" Enable spellchecking
set spell

" unicode
set encoding=utf-8              " best default encoding
setglobal fileencoding=utf-8    " ...
set nobomb                      " do not write utf-8 BOM!
EOF

ln -s ../$(realpath out)/bin/cyanide docker-rootfs/bin/cyanide
ln -s ../$(realpath out)/bin/vim docker-rootfs/bin/vim
