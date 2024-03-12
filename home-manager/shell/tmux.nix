{ pkgs, tmux-nvim-src, ... }:
let
	tmux-nvim = pkgs.tmuxPlugins.mkTmuxPlugin
		{
			pluginName = "tmux.nvim";
			version = "unstable-2023-01-06";
			src = tmux-nvim-src;
		};

in {
	programs.tmux = {
		enable = true;
		terminal = "tmux-256color";
		plugins = with pkgs;
			[
				tmux-nvim
				tmuxPlugins.sensible
				tmuxPlugins.catppuccin
			];
		extraConfig = ''
			set -g default-terminal "tmux-256color"
			set -ag terminal-overrides ",xterm-256color:RGB"

			set -g @catppuccin_window_default_text "#W"
			set -g @catppuccin_window_current_text "#W"

			set -g @catppuccin_window_left_separator "█"
			set -g @catppuccin_window_right_separator "█ "
			set -g @catppuccin_window_middle_separator "  █"
			set -g @catppuccin_window_number_position "right"
			set -g @catppuccin_window_default_fill "number"
			set -g @catppuccin_window_current_fill "number"

			set -g @catppuccin_status_modules_right "directory date_time battery"
			set -g @catppuccin_status_left_separator  " "
			set -g @catppuccin_status_right_separator ""
			set -g @catppuccin_status_right_separator_inverse "no"
			set -g @catppuccin_status_fill "icon"
			set -g @catppuccin_status_connect_separator "no"
			set -g @catppuccin_date_time_text "%H:%M"

			unbind C-b
			set-option -g prefix M-x
			bind M-x send-prefix
			
			bind r source-file ~/.config/tmux/tmux.conf

			bind h split-window -h
			bind v split-window -v

			bind k killw

			bind -n M-[ previous-window
			bind -n M-] next-window
		'';
	};
}
