#!/usr/bin/env bash

# symlinking stuff if they arent symlinked already

if [ ! -d "$HOME/.config/doom/" ]; then
  echo "[i] Creating symlink for Doom Emacs"
  ln -s /etc/nixos/nonnix/doom/ ~/.config/
fi

if [ ! -d "$HOME/.config/nvim/" ]; then
  echo "[i] Creating symlink for Neovim"
  ln -s /etc/nixos/nonnix/nvim/ ~/.config/
fi

if [ ! -d "$HOME/.config/polybar" ]; then
  echo "[i] Creating symlink for Polybar"
  ln -s /etc/nixos/nonnix/polybar/ ~/.config/
fi

if [ ! -d "$HOME/.config/qtile/" ]; then
  echo "[i] Creating symlink for Qtile"
  ln -s /etc/nixos/nonnix/qtile/ ~/.config/
fi

if [ ! -d "$HOME/.config/rofi/" ]; then
  echo "[i] Creating symlink for Rofi"
  ln -s /etc/nixos/nonnix/rofi/ ~/.config/
fi
