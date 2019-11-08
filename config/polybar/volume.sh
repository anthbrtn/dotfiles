#!/usr/bin/env bash

# Terminate already running volumecontrol instances
killall -q volumeicon

# Wait until the processes have been shut down
while pgrep -u $UID -x volumeicon >/dev/null; do sleep 1; done

# Launch volumeicon, using default config location ~/.config/polybar/config
volumeicon
