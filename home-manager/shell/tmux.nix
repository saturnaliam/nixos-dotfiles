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
