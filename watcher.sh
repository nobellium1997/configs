#!/bin/bash
function watcher {
    inotifywait -e modify -r $1 -m \
        --excludei "\.(dll|cache|swp|git|gitignore)" |
        while read path action file; do
            if [[ "$path" == *"obj"* || "$path" == *"bin"* ]]; then
                echo "ignored"
            else
                echo "Transferring $path$file"
                local sub_path=$(echo $path | sed "s:/home/nobel/Work/Services/::g")
                scp "$path$file" nobel.barakat@10.0.0.158:C:/Users/nobel.barakat/Work/Services/"$sub_path$file"
            fi

        done
}

watcher $1
