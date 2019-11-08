#!/usr/bin/env bash

# Terminate already running powermanager applet instances
killall -q xfce4-power-manager

# Wait until the processes have been shut down
while pgrep -u $UID -x xfce4-power-manager >/dev/null; do sleep 1; done

# Launch power manager applet
xfce4-power-manager
