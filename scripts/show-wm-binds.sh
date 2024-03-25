#!/usr/bin/env bash
cat /etc/nixos/home-manager/wayland/hyprland.nix |\
	sed -n "/bind/,/]/p" |\
	sed -e "s/[\t|\"]//g" \
	-e "s/bind = \[//g" \
  -e "s/  //g" \
	-e "s/\];//g" \
	-e "s/\$mod,/SUPER +/g" \
	-e "s/\$mod SHIFT,/SUPER + SHIFT +/g" \
	-e "/^$/d" \
	-e "/^#/d" \
  -e "s/code:61/\?/g" \
	-e "s/,.*#/ -/g" |\
  rofi -dmenu -theme /etc/nixos/nonnix/rofi/keybinds.rasi -p "keybinds"
