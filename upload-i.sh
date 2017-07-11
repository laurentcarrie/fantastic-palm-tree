#!/bin/sh

read i

echo "#!/bin/sh
" > cmd.txt
echo $i >> cmd.txt
./upload-cmd.sh cmd.txt $1


