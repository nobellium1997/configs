#!/bin/zsh

# Custom arrow keys
autokey &

# Clipboard persistence
parcellite &

# Swap ctrl with caps lock 
~/initscript.sh &

# Set background
feh --bg-fill ~/dotfiles/wallpapers/newwall.png &

# Run clipboard manager daemon 
/home/nobel/SuckLess/clipmenu/clipmenud &

# Settings for jetbrain products
export _JAVA_AWT_WM_NONREPARENTING=1 
export AWT_TOOLKIT=MToolkit 
wmname LG3D

# Set status bar to display time and battery 
while true; do
	xsetroot -name " $(amixer -c 1 sget Mic | tail -1) | $(acpi | rg "Battery 0") | $(date +'%D %H:%M') "
	sleep 1
done &
