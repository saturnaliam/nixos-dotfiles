{ config, pkgs, ... }:
{
	programs.fish = {
		enable = true;
		interactiveShellInit = ''
			set fish_greeting
			set -g __fish_git_prompt_showdirtystate 1

			function fish_prompt
				printf ' %s%s@%s %s%s%s%s %s><> ' (set_color red) (whoami) (prompt_hostname) (set_color $fish_color_command) (prompt_pwd) (set_color $fish_color_cwd) (fish_git_prompt) (set_color $fish_color_keyword)
			end

      set -x MANPAGER "nvim +Man!"
		'';

		shellAliases = {
			rm = "rm -i";
			ls = "exa -a --color=always --group-directories-first";
			l = "exa -a --color=always --group-directories-first";
			ll = "exa -al --color=always --group-directories-first";
			mkdir = "mkdir -p";
			rebuild = "cd /etc/nixos && git add -f hardware.nix && sudo nixos-rebuild switch && git rm -r --cached hardware.nix && cd -"; # do not look
			gaa = "git add --all";
			ga = "git add";
			gm = "git commit -m";
			gp = "git push";
			gl = "git pull";
			gcl = "git clone";
			gch = "git checkout";
			gb = "git branch";
			gs = "git status";
		};
	};
}
