#!/bin/zsh

# Arandr
# ~/.screenlayout/two_screen.sh &

# Set background
feh --bg-fill ~/Wallpapers/oldest_house.jpg &

# Run clipboard manager daemon 
/home/nobel/Suckless/clipmenu/clipmenud &

# Raise keyboard rate
xset r rate 279 40 &

dwmblocks &
