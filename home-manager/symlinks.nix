{ config, ... }:
{
  home.file = {
    ".config/doom".source = config.lib.file.mkOutOfStoreSymlink ../nonnix/doom;
    ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink ../nonnix/nvim;
    ".config/rofi".source = config.lib.file.mkOutOfStoreSymlink ../nonnix/rofi;
    ".config/tmux".source = config.lib.file.mkOutOfStoreSymlink ../nonnix/tmux;
  };
}
