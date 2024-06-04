{ pkgs, ... }: {
  imports =
    [ ./kitty.nix ./alacritty.nix ./fish.nix ./tmux.nix ./starship.nix ];
}
