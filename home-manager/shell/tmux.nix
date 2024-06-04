{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    historyLimit = 10000;
    plugins = with pkgs;
      [
        pkgs.tmuxPlugins.sensible
        pkgs.tmuxPlugins.vim-tmux-navigator
        pkgs.tmuxPlugins.battery
        {
          plugin = pkgs.tmuxPlugins.catppuccin;
          extraConfig = ''
            set -g @catppuccin_window_default_text "#W"
            set -g @catppuccin_window_current_text "#W"
            set -g @catppuccin_window_left_separator "█"
            set -g @catppuccin_window_right_separator "█ "
            set -g @catppuccin_window_middle_separator "  █"
            set -g @catppuccin_window_number_position "right"
            set -g @catppuccin_window_default_fill "number"
            set -g @catppuccin_window_current_fill "number"            
            set -g @catppuccin_status_modules_right "directory date_time"
            set -g @catppuccin_status_left_separator  " "
            set -g @catppuccin_status_right_separator ""
            set -g @catppuccin_status_right_separator_inverse "no"
            set -g @catppuccin_status_fill "icon"
            set -g @catppuccin_status_connect_separator "no"
            set -g @catppuccin_date_time_text "%H:%M"
          '';
        }
      ];

      extraConfig = ''
        set -g mouse on

        unbind C-b
        set -g prefix M-x
        bind M-x send-prefix

        bind -n M-[ previous-window
        bind -n M-] next-window

        bind h split-window -h -c "#{pane_current_path}"
        bind v split-window -v -c "#{pane_current_path}"

        bind k killw
      '';
  };
}
