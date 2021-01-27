#!/bin/bash
set -e
function getuser {
    source ~/\.passwords/bw_pass
    local NAME=$(bw list items --nointeraction | jq '.[].name' | dmenu -i)
    local NAME_STRIPPED=$(echo $NAME | sed 's/"//g')
    local USER=$(bw list items --search "$NAME_STRIPPED" --nointeraction | jq '.[0].login.username' | sed 's/"//g')
    echo $USER | xclip -sel clip
}

getuser
