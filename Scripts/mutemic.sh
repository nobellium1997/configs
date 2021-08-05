#!/bin/bash
# amixer set -c1 Mic toggle
amixer -D pulse sset Capture toggle
mpg123 ~/Downloads/me-too-603.mp3
