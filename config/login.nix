{ pkgs, catppuccin-sddm-src, ... }:
let
  catppuccin-mocha = pkgs.stdenv.mkDerivation {
    name = "catppuccin-mocha";
    src = "${catppuccin-sddm-src}/src/catppuccin-mocha";
    installPhase = ''
      mkdir -p $out/share/sddm/themes/catppuccin-mocha
      mv * $out/share/sddm/themes/catppuccin-mocha
    '';
  };
in {
  environment.systemPackages = with pkgs.libsForQt5; [
    catppuccin-mocha
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
      theme = "catppuccin-mocha";
    };
  };
}
