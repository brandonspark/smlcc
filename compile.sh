#!/bin/bash

ml-build sources.cm Test.main compile > /dev/null

var1=`dirname $1`
var2=`basename $1`

smlnj @SMLload compile.amd64-linux $var1 $var2 > /dev/null

newname="${var1}/${var2:0:-2}.s"
gcc -m32 $newname -o "${newname:0:-2}" > /dev/null

rm $newname
