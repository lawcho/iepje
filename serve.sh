#!/usr/bin/env bash

# Serve the compiled gallery over HTML

# Backport https://github.com/emikulic/darkhttpd/pull/84
echo "text/javascript mjs" >_build/extramime

darkhttpd web --mimetypes _build/extramime $@
