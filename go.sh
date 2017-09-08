#!/bin/sh

set -e
set -x 


cd /home/ubuntu

#rm -rf fantastic-palm-tree
rm -rf fantastic-palm-tree/src

aws s3 sync s3://lolo-web/fantastic-palm-tree fantastic-palm-tree


cd fantastic-palm-tree
rm -rf build
mkdir -p build
cd build
cmake ../src
ls
make

cat <<EOF > data.txt
{
"filename":"../songs/muse/starlight.song"
}
EOF

./f4242 data.txt

#ls -R

aws s3 cp ../songs/muse/starlight.tex s3://lolo-web/starlight.tex

