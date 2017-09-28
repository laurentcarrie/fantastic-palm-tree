#!/bin/sh

set -e
set -x 

export HOME=/home/ubuntu

export PROJECT_HOME=$HOME/fantastic-palm-tree
export BUILD_HOME=$HOME/fantastic-palm-tree/build

cd $HOME

rm -rf fantastic-palm-tree
rm -rf fantastic-palm-tree/src

aws s3 sync s3://lolo-web/fantastic-palm-tree $PROJECT_HOME

if false ; then
apt-get install -y libextlib-ocaml-dev libpcre-ocaml-dev  libcryptokit-ocaml-dev 
fi

cd $PROJECT_HOME

ls -R

find $PROJECT_HOME -name "*.cpp" | while read f ; do
    rm $f 
done

find $PROJECT_HOME -name "*.h" | while read f ; do
    rm $f 
done

rm -rf $BUILD_HOME
mkdir -p  $BUILD_HOME
cd $BUILD_HOME

bash $PROJECT_HOME/oboot/bootstrap prefix=$BUILD_HOME/install

omake pdf

ls -R install

#aws s3 cp $BUILD_HOME/others/time.tex s3://lolo-web/time.tex
aws s3 sync $BUILD_HOME s3://lolo-web/yyy

