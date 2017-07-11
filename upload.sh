#!/bin/sh

f=$1

aws s3 cp $f s3://lolo-web/$f

