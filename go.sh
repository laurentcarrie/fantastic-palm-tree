#!/bin/sh

rm -f song-to-html
ocamlfind ocamlopt -o song-to-html read.ml -linkpkg -package extlib
