#!/bin/sh

set -e
set -x 


cd /home/ubuntu

rm -rf fantastic-palm-tree
rm -rf fantastic-palm-tree/src

aws s3 sync s3://lolo-web/fantastic-palm-tree fantastic-palm-tree


cd fantastic-palm-tree
rm -rf build
mkdir -p build
cd build
cmake ../src
ls
make

cmake --graphviz=m.dot ../src
ls -lhtr
aws s3 cp m.dot s3://lolo-web/m.dot

cat <<EOF > data.txt
{
"builddir":"/home/ubuntu/fantastic-palm-tree/build",
"srcdir":"/home/ubuntu/fantastic-palm-tree/songs",
"filename":"muse/starlight.song"
}
EOF

mkdir -p /home/ubuntu/fantastic-palm-tree/build/muse

./f4242 data.txt

ls -R /home/ubuntu/fantastic-palm-tree/build

cd /home/ubuntu/fantastic-palm-tree/build/muse

ls starlight*.mp | while read f ; do
    aws s3 cp $f s3://lolo-web/xxx/$f
    b=`basename $f .mp`
    mpost $f
    mv $b.1 $b.mps
done

aws s3 cp starlight.tex s3://lolo-web/xxx/starlight.tex


pdflatex starlight.tex
pdflatex starlight.tex
ls -lhtr
aws s3 cp starlight.pdf s3://lolo-web/xxx/starlight.pdf


cd /home/ubuntu/fantastic-palm-tree



