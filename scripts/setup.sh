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

# seting up go for doom emacs
go install github.com/motemen/gore/cmd/gore@latest
go install github.com/stamblerre/gocode@latest
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install github.com/cweill/gotests/...@latest
go install github.com/fatih/gomodifytags@latest

go install golang.org/x/tools/gopls@latest

go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
