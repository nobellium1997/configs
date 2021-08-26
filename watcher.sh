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

SUB_PATH=$(echo $1 | sed "s:/home/nobel/Work/Services/::g")
FULL_PATH="C:/Users/nobel.barakat/Work/Services/$SUB_PATH"

echo "Deleting $FULL_PATH..."
ssh nobel.barakat@10.0.0.53 Remove-Item "$FULL_PATH" -Recurse -Force -Confirm:\$false
echo "Deleted"

echo "Creating $FULL_PATH/application_csharp..."
ssh nobel.barakat@10.0.0.53 mkdir "$FULL_PATH"
echo "Created"

scp -r "$1/application_csharp" "nobel.barakat@10.0.0.53:$FULL_PATH/application_csharp"
watcher "$1/application_csharp"
