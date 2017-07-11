#!/bin/sh

set -e 

encore=true
index=$1
f=done${index}.txt

while ${encore} ; do
ret=`aws s3 ls s3://lolo-web/$f | wc --lines`
if [ $ret -eq 1 ] ; then
aws s3 mv s3://lolo-web/$f $f
cat $f
exit 0
else
echo "waiting for result..."
fi

sleep 5
done