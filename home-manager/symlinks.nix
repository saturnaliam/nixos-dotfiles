{ config, ... }:
{
  home.file = {
    ".config/doom".source = ../nonnix/doom;
    ".config/nvim".source = ../nonnix/nvim;
    ".config/rofi".source = ../nonnix/rofi;
    ".config/qtile".source = ../nonnix/qtile;
  };
}
