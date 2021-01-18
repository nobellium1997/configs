#!/bin/bash
amixer -c1 get Mic | rg "\[on\]" >/dev/null
MIC_IS_ON=$(echo $?)

MIC_MESSAGE="Mic is OFF"
if [ "$MIC_IS_ON" -eq 0 ]; then
    MIC_MESSAGE="Mic is LIVE"
fi

echo $MIC_MESSAGE
