#!/bin/sh

f=$1
i=$2

rm -f done$2.txt
aws s3 rm s3://lolo-web/done$2.txt ||:
aws s3 cp $f s3://lolo-web/whatcmd$2.txt

