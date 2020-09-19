#!/bin/zsh

# Set the screen layout
~/.screenlayout/res.sh &

# Clipboard persistence
parcellite &

# Swap ctrl with caps lock 
~/initscript.sh &

# Set background
feh --bg-fill ~/Wallpapers/samurai.png &

# Run clipboard manager daemon 
/home/nobel/SuckLess/clipmenu/clipmenud &

# Settings for jetbrain products
export _JAVA_AWT_WM_NONREPARENTING=1 &
export AWT_TOOLKIT=MToolkit &
wmname LG3D &

# Start Compton for compositing effects
compton &

# Set status bar to display time and battery
while true; do
	LOAD_AVG=$(cat /proc/loadavg | awk -F ' ' '{print $1}')

	xsetroot -name " Load Avg: $LOAD_AVG | $(acpi | rg "Battery 0" | sed "s/Battery 0: //g") | $(date +'%D %H:%M') "
	sleep 1
done &
