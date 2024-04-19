#!/usr/bin/env bash
pkill polybar
polybar &

picom --daemon &
emacs --daemon &
nitrogen --restore &
