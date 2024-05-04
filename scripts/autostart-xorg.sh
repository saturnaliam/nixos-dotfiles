#!/usr/bin/env bash
nitrogen --restore &
syncthing serve &
emacs --daemon &
