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

# Touchpad: Tweak settings.
#
# For an explanation of these commands
# see [1] and [2].
#
# I have noticed that xinput device IDs
# and device property IDs can change across
# system reboots so it better to use
# device names and property names rather
# than IDs. It is also easier to understand
# what the commands are doing.
#
# [1]: https://askubuntu.com/questions/172972/configure-mouse-speed-not-pointer-acceleration
# [2]: https://wiki.archlinux.org/title/libinput

touchpad_name=$(xinput --list --name-only | grep -i touchpad)
xinput list-props "$touchpad_name"

# Generate mouse clicks when tapping the touchpad with one/two/three fingers.
xinput set-prop "$touchpad_name" "libinput Tapping Enabled" 1

# Speed up mouse cursor movement while dragging finger(s) on touchpad.
xinput set-prop "$touchpad_name" "libinput Accel Speed" 0.75

# Start stumpwm as main window manager process.
exec ssh-agent /home/benv/.guix-profile/bin/stumpwm
