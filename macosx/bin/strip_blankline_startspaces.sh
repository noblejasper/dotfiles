#!/bin/sh

filename=${1}
str=`cat $filename`;

echo $str | sed "s|^[ ]*||g" | sed "s|\n\n|\n|g"