#!/usr/bin/env bash

# Terminate already running network manager applet instances
killall -q nm-applet

# Wait until the processes have been shut down
while pgrep -u $UID -x nm-applet >/dev/null; do sleep 1; done

# Launch network manager applet
nm-applet
