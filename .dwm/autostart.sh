#!/bin/zsh

# Custom arrow keys
autokey &

# Clipboard persistence
parcellite &

# Swap ctrl with caps lock 
~/initscript.sh &

# Set background
feh --bg-fill ~/dotfiles/wallpapers/PixelCity.png &

# Run clipboard manager daemon 
/home/nobel/SuckLess/clipmenu/clipmenud &

# Settings for jetbrain products
export _JAVA_AWT_WM_NONREPARENTING=1 &
export AWT_TOOLKIT=MToolkit &
wmname LG3D &

# Start Compton for compositing effects
compton &

# Start bwall
# bwall -pixel_city &

# Set status bar to display time and battery
while true; do
	amixer -c 1 sget Mic | rg "\[on\]"
	MIC_IS_ON=$(echo $?)

	MIC_MESSAGE="Mic is OFF"
	if [ "$MIC_IS_ON" -eq 0 ]; then
		MIC_MESSAGE="Mic is LIVE"
	fi

	xsetroot -name " $MIC_MESSAGE | $(acpi | rg "Battery 0") | $(date +'%D %H:%M') "
	sleep 1
done &
