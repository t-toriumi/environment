#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
DOT_FILE=($(ls | grep "^dot"))

for i in ${DOT_FILE[@]}
do
  ln -siv $SCRIPT_DIR/$i ~/${i/dot/}
done
