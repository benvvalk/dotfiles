#!/usr/bin/env bash

eval `gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh`
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

# turn off screensaver
xset s off

# turn off bell
xset b off

# set keyboard repeat rate / repeat delay
xset r rate 250 60

# I test if EXWM is defined inside my init.el
# to decide if I should start exwm on emacs startup.
export EXWM=1

exec emacs --maximized --debug-init
