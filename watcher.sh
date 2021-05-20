#!/bin/bash
function watcher {
    inotifywait -e modify -r $1 -m \
        --excludei "\.(dll|cache|swp|git|gitignore)" |
        while read path action file; do
            if [[ "$path" == *"obj"* || "$path" == *"bin"* ]]; then
                echo "ignored"
            else
                local sub_path=$(echo $path | sed "s:/home/nobel/Work/Services/::g")
                echo "copying $sub_path$file"
                scp "$path$file" nobel.barakat@10.0.0.53:C:/Users/nobel.barakat/Work/Services/"$sub_path$file"
            fi

        done
}

watcher $1
