#!/usr/bin/env bash

# Terminate already running network manager applet instances
killall -q blueman-applet

# Wait until the processes have been shut down
while pgrep -u $UID -x blueman-applet >/dev/null; do sleep 1; done

# Launch network manager applet
blueman-applet
