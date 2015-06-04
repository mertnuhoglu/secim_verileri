#!/bin/bash

filename=$1
basename=${filename##*/}
target_dir=memurlarnet/clean
mkdir -p $target_dir
target=$target_dir/$basename

sed '1 d' $filename | sed '1 i\
{ "secim": [
' | sed "$ a\
}
" | iconv -f ISO-8859-9 -t UTF-8 > $target
