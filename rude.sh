#!/bin/sh

set -e
set -x 


export H=/home/ubuntu/work

mkdir -p $H
cd $H


#wget http://rudeserver.com/socket/download/rudesocket-1.2.0.tar.gz --output-document rudesocket-1.2.0.tar.gz

tar xvzf rudesocket-1.2.0.tar.gz
cd rudesocket-1.2.0
./configure --with-openssl
make
make install

ls -l



