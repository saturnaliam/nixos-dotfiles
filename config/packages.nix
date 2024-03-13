{ pkgs, ... }:
{
	environment.systemPackages = with pkgs; [
		neovim
		git
		neofetch
		firefox
		tmux
		grim
		slurp
		swappy
		discord
		keepassxc
		rofi-wayland-unwrapped
		gh
		gnupg
	];

	programs.hyprland.enable = true;

	programs.fish.enable = true;

	services.emacs.enable = true;
	services.emacs.package = pkgs.emacs29.override {
		withPgtk = true;
	};

	programs.direnv.enable = true;
	programs.direnv.nix-direnv.enable = true;
	programs.direnv.silent = true;
}