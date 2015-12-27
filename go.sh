#!/bin/sh

set -e
set -x 

rm -f song-to-html
ocamlfind ocamlopt -warn-error all  -o song-to-html read.ml -linkpkg -package extlib,str

songs=perfect-day

mkdir -p install

cp song.css install/.
cp song.css ~/Dropbox/zik/partoches/.
cp index.html ~/Dropbox/zik/partoches/.
for s in $songs ; do
    ./song-to-html $s.song install/$s.html
    cp install/$s.html ~/Dropbox/zik/partoches/.
done