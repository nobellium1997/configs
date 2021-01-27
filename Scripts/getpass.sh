#!/bin/bash
set -e
function getpass {
    source ~/\.passwords/bw_pass
    local NAME=$(bw list items --nointeraction | jq '.[].name' | dmenu -i)
    local NAME_STRIPPED=$(echo $NAME | sed 's/"//g')
    local PASS=$(bw list items --search "$NAME_STRIPPED" --nointeraction | jq '.[0].login.password' | sed 's/"//g')
    echo $PASS | xclip -sel clip
}

getpass
