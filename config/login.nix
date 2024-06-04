{ pkgs, ... }: {
  environment.systemPackages = with pkgs.libsForQt5; [
    qtbase
    qtsvg
    qtquickcontrols2
    qtgraphicaleffects
  ];

  services.xserver = { enable = true; };

  services.displayManager.sddm = {
    package = pkgs.kdePackages.sddm;
    enable = true;
    theme = "catppuccin-mocha";
  };
}
