{ pkgs, ... }:
{
	imports = [
		./kitty.nix
#		./tmux.nix
		./alacritty.nix
	];

	programs.fish.enable = true;
#	programs.tmux.enable = true;
}
