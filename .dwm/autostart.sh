#!/bin/zsh

# Arandr
~/.screenlayout/single.sh &

# Set background
feh --bg-fill ~/Wallpapers/quarry.jpeg &

# Run clipboard manager daemon 
/home/nobel/Suckless/clipmenu/clipmenud &

# Raise keyboard rate
xset r rate 279 40 &

setxkbmap -option caps:swapescape &

compton &

dwmblocks &
