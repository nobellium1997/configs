#!/bin/bash
inotifywait -e modify -r ~/temp -m |
    while read path action file; do
        echo "Transferring $file"
        # rsync $file nobel.barakat@10.0.0.53:C:\Users\nobel.barakat\work
    done
