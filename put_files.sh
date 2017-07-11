#!/bin/sh

function putfile {
f=$1
aws s3 cp $f s3://lolo-web/CIF/$f
}

source ./files.txt

aws s3 cp files.txt s3://lolo-web/files.txt

for f in $files ; do
putfile $f
done



