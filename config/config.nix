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

  programs.nix-ld.enable = true;

  networking.hostName = "";
  networking.dhcpcd.enable = true;
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.scanRandMacAddress = false;
  
  services.pipewire.enable = true;
  services.pipewire.pulse.enable = true;
  services.pipewire.wireplumber.enable = true;

  hardware.opengl.enable = true;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      monaspace
      terminus-nerdfont
      jetbrains-mono  
      (nerdfonts.override { fonts = [ "Iosevka" "Monaspace" "JetBrainsMono" "NerdFontsSymbolsOnly" ]; })
      font-awesome
      emacs-all-the-icons-fonts
    ];
    
    fontconfig = {
      enable = true;
    };
  };

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  hardware = {
    nvidia.modesetting.enable = true;
  };

  nix.settings.auto-optimise-store = true;  

  nixpkgs.config.allowUnfree = true;
  
  system.stateVersion = "23.11";
  
  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
  };
}
