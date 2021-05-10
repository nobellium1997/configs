#!/usr/bin/env bash
set -euo pipefail

CURRENT_PROFILE=$(pactl info | grep "Default Sink" | awk -F ' ' '{print $3}')

if [ "$CURRENT_PROFILE" = "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" ] ; then
        pacmd set-default-sink "alsa_output.usb-Logitech_G533_Gaming_Headset-00.analog-stereo"
else
        pacmd set-default-sink "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor"
fi
