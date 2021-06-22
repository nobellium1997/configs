#!/usr/bin/env bash
set -euo pipefail

#!/bin/bash
intern=eDP-1
extern=HDMI-1

if xrandr | grep "$extern disconnected"; then
    xrandr --output "$extern" --off --output "$intern" --mode 1920x1080
else
    xrandr --output "$intern" --off --output "$extern" --mode 1920x1080
fi
