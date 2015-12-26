#!/bin/sh

set -e
set -x 

rm -f song-to-html
ocamlfind ocamlopt -warn-error all  -o song-to-html read.ml -linkpkg -package extlib

songs=perfect-day

for s in $songs ; do
    ./song-to-html $s.song $s.html
done