{ pkgs, ... }:
{
	nix.package = pkgs.nixFlakes;
	nix.extraOptions = ''
		experimental-features = nix-command flakes ca-derivations
		restrict-eval = false
	'';
	
	users.mutableUsers = false;
	users.users.lucia = {
		isNormalUser = true;
		description = "the lucia account";
		extraGroups = [ "wheel" "networkmanager" ];
		shell = pkgs.fish;
		hashedPasswordFile = "/etc/nixos/password";
	};

	time.timeZone = "America/Chicago";	

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	networking.hostName = "nixos";
	networking.wireless.enable = false;
	networking.networkmanager.enable = true;
	
	services.pipewire.enable = true;
	services.pipewire.pulse.enable = true;
	services.pipewire.wireplumber.enable = true;

	hardware.opengl.enable = true;

	fonts = {
		enableDefaultPackages = true;
		packages = with pkgs; [
			jetbrains-mono	
			(nerdfonts.override { fonts = [ "JetBrainsMono" "NerdFontsSymbolsOnly" ]; })
			font-awesome
			emacs-all-the-icons-fonts
		];
		
		fontconfig = {
			enable = true;
		};
	};

	nix.settings.auto-optimise-store = true;	

	nixpkgs.config.allowUnfree = true;
	
	system.stateVersion = "23.11";
}
