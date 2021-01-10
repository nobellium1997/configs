#!/bin/zsh

# Arandr
~/.screenlayout/single.sh &

# Set background
feh --bg-fill ~/Wallpapers/oldest_house.jpg &

# Run clipboard manager daemon 
/home/nobel/Suckless/clipmenu/clipmenud &

# Raise keyboard rate
xset r rate 279 40 &

# Set status bar to display time and battery
while true; do
	amixer -c 3 sget Mic | rg "\[on\]"
	MIC_IS_ON=$(echo $?)

	MIC_MESSAGE="Mic is OFF"
	if [ "$MIC_IS_ON" -eq 0 ]; then
		MIC_MESSAGE="Mic is LIVE"
	fi

	LOAD_AVG=$(cat /proc/loadavg | awk -F ' ' '{print $1}')
    BATT=$(acpi)
	NETWORK=$(nmcli -t -f name connection show --active)

	xsetroot -name " $NETWORK | $BATT | Load Avg: $LOAD_AVG | $MIC_MESSAGE | $(date +'%D %H:%M') "
	sleep 1
done &
