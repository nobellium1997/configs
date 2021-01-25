#!/bin/bash
set -e
source ~/\.passwords/bw_pass
local NAME=$(bw list items | jq '.[].name' | dmenu)
local NAME_STRIPPED=$(echo $NAME | sed 's/"//g')
local USER=$(bw get username $NAME_STRIPPED)
echo $USER | xclip -sel clip
