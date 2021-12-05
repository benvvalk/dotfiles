#! /bin/bash

eval `gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh`
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

# change default font settings (default font is too small
# on Framework laptop's high-DPI screen).
xrdb -merge ~/.Xresources

# change mouse cursor appearance
xsetroot -cursor_name left_ptr

# turn off screensaver
xset s off

# turn off bell
xset b off

# Touchpad: Increase mouse cursor speed/acceleration.
#
# * `12` is the device id for the touchpad
# * `323` is the property id for mouse acceleration
# * `0.5` is the new value to assign
#
# It seems like the assigned value (0.5 in this case)
# must be between 0.0 and 1.0, with default being
# 0.0. For info about how to determine device ids and
# property id, see:
#
# https://askubuntu.com/questions/172972/configure-mouse-speed-not-pointer-acceleration

xinput set-prop 12 323 0.5

# Touchpad: Enable tap-to-click.

xinput set-prop 12 332 1

exec ssh-agent /home/benv/.guix-profile/bin/stumpwm
