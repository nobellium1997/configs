#!/bin/bash
set -e
source ~/\.passwords/bw_pass
local NAME=$(bw list items | jq '.[].name' | dmenu)
local NAME_STRIPPED=$(echo $NAME | sed 's/"//g')
local PASS=$(bw get password $NAME_STRIPPED)
echo $PASS | xclip -sel clip
