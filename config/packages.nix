{ pkgs, ... }:
{
	environment.systemPackages = with pkgs; [
		neovim
		obs-studio
		git
		neofetch
		firefox
		tmux
		grim
		slurp
		swappy
		discord
		keepassxc
		eza
		rofi-wayland-unwrapped
		gh
		gnupg
		ripgrep
		fd
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

	xdg.portal.enable = true;
	xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
}
