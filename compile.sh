#!/usr/bin/env bash

# Script to build the Iepje examples gallery

AGDA_SRC=src/Iepje/Examples/Gallery.agda
OTHER_SRC="src/Iepje/Examples/index.html src/Iepje/Examples/style.css"

set -x
mkdir -p web/
agda $AGDA_SRC --js --js-es6 --compile-dir=web/js/
agda $AGDA_SRC --html --html-dir=web/html/
cp $OTHER_SRC web/
