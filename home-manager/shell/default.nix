{ pkgs, ... }:
{
	imports = [
		./kitty.nix
#		./tmux.nix
		./alacritty.nix
	];

#	programs.tmux.enable = true;
}
