#!/usr/bin/env bash

# Script to pretty-print the Agda source code of Iepje
# to highlighted SVG, which can be imported into Inkscape

# Every line of code is wrapped in a coloured div, so that Inkscape's
# bounding-box snapping can be used to move chunks of code around
# whilst preserving precise spacing & alignment

set -euxo pipefail

mkdir -p _build/html/raw _build/html/lined _build/svg

# Generate HTML, without surrounding boilerplate
agda ${AGDA_SRC:-src/Iepje/Examples/Gallery.agda} \
  --html --html-dir=_build/html/raw/ --html-highlight=code

# Tweak the Agda.css file to make the line-boxes visible
cat _build/html/raw/Agda.css svg_style.css >_build/html/lined/Agda.css

for file in $(cd _build/html/raw/ && echo *.html); do

  # Modify the HTML to wrap each line in a span
  cat <<EOF >_build/html/lined/$file
<!DOCTYPE html>
<meta charset="utf-8">
<link rel="stylesheet" href="Agda.css">
<pre class="Agda">
$(sed 's;.*;<span class="line">\0</span>;' <_build/html/raw/$file)
</pre>
EOF

  # Convert the HTML to SVG
  wkhtmltoimage --enable-local-file-access _build/html/lined/$file _build/svg/${file%.html}.svg
done
