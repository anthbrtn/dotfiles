#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar, using default config location ~/.config/polybar/config

echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar leftbar >>/tmp/polybar1.log 2>&1 &
polybar rightbar >>/tmp/polybar2.log 2>&1 &

# Launch the system tray applets that are normally executed by
# i3. This is done in this script, as opposed to i3, in order to
# ensure that their transparent backgrounds are respected and
# that they are launched after polybar is.

sh ./volume.sh
sh ./power.sh
sh ./networkmanager.sh
sh ./bluetooth.sh
