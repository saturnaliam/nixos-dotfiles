{ config, ... }:
{
  home.file = {
    ".config/doom".source = ../nonnix/doom;
    ".config/nvim".source = ../nonnix/nvim;
    ".config/rofi".source = ../nonnix/rofi;
#    ".config/tmux".source = ../nonnix/tmux;
  };
}
