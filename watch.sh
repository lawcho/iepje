#!/usr/bin/env bash

# Top-level development script.

# Serve gallery. Server dies alongside this script.
./serve.sh --port 8080 &

# Open web browser. Browser outlives this script.
xdg-open http://localhost:8080/ & disown

# Rebuild gallery once, then again whenever src/ changes.

# inotifywait -r is more robust than entr (which crashes when files are deleted)
inotifywait -mre "attrib,close_write,move,move_self,create,delete,delete_self" src/ |
while
  ./compile.sh;
  read line;
do true; done
# https://stackoverflow.com/questions/24420342/is-there-a-do-while-loop-in-bash
