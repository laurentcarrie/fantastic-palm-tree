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


cat <<EOF > data.txt
{
"builddir":"/home/ubuntu/fantastic-palm-tree/tex-songs",
"srcdir":"/home/ubuntu/fantastic-palm-tree/songs",
"filename":"muse/starlight.song"
}
EOF

mkdir -p /home/ubuntu/fantastic-palm-tree/tex-songs

mkdir -p /home/ubuntu/fantastic-palm-tree/build/muse

./f4242 data.txt

rm -rf core*

ls -R /home/ubuntu/fantastic-palm-tree/tex-songs

aws s3 sync /home/ubuntu/fantastic-palm-tree/tex-songs s3://lolo-web/tex-songs

cd /home/ubuntu/fantastic-palm-tree/tex-songs/muse

if 0; then
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
fi


aws s3 cp /home/ubuntu/fantastic-palm-tree/tex-songs/CMakeLists.txt s3://lolo-web/CMakeLists.txt

echo "XXXXXXXXXXXXXXXXXX1"

#aws s3 sync /home/ubuntu/fantastic-palm-tree/build s3://lolo-web/xxxbuild

echo "XXXXXXXXXXXXXXXXXX2"

cd /home/ubuntu/fantastic-palm-tree/tex-songs
cp /home/ubuntu/fantastic-palm-tree/UseLATEX.cmake .
ls -R
echo "XXXXXXXXXXXXXXXXXX3"
cmake $PWD
echo "XXXXXXXXXXXXXXXXXX4"
ls -R
echo "XXXXXXXXXXXXXXXXXX5"
make -k || echo "build failed"
echo "XXXXXXXXXXXXXXXXXX6"

aws s3 sync /home/ubuntu/fantastic-palm-tree/xxx-pdf s3://lolo-web/xxx-pdf

ls -R install
aws s3 sync /home/ubuntu/fantastic-palm-tree/install s3://lolo-web/install




