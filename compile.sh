#!/usr/bin/env bash

SOURCE=src/Iepje/Examples/Gallery.agda

echo Compiling examples to JS
agda $SOURCE --js --js-es6 --compile-dir=_build/js/
echo Compiling examples to HTML
agda $SOURCE --html --html-dir=_build/html/
