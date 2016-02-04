#!/bin/sh

set -e
set -x 

rm -rf bin
mkdir -p bin
ocamlfind ocamlopt -warn-error all  -o bin/song-to-html read.ml -linkpkg -package extlib,str

songs=perfect-day

mkdir -p install
cp css/song.css install/.

./bin/song-to-html $PWD/songs $PWD/install

zip -r partoches.zip install
cp $PWD/partoches.zip  ~/Dropbox/partoches.zip

auto-ftp --hostname pixies --port 1024 --user laurent  < ftp-commands.txt || echo failed

