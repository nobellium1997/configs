#!/bin/bash
set -e
function getuser {
    source ~/\.passwords/bw_pass
    local NAME=$(bw list items | jq '.[].name' | dmenu -i)
    local NAME_STRIPPED=$(echo $NAME | sed 's/"//g')
    local USER=$(bw get username $NAME_STRIPPED)
    echo $USER | xclip -sel clip
}

getuser