{ pkgs, ... }:
{
	environment.systemPackages = with pkgs.libsForQt5; [
		pkgs.catppuccin-sddm-corners
		qtbase
		qtsvg
		qtquickcontrols2
		qtgraphicaleffects	
	];
	
	services.xserver = {
		enable = true;

		displayManager.defaultSession = "hyprland";
		displayManager.sddm = {
			enable = true;
			theme = "catppuccin-sddm-corners";
		};
	};
}
