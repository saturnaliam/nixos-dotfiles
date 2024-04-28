{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # various things to make using the wm easier
    grim   
    slurp
    picom
    polybar
    swappy
    rofi-wayland-unwrapped
    gtk3
    nitrogen

    # text editors + required stuff for them
    helix
    ripgrep
    libreoffice-fresh # this is more than a text editor but idc
    fd
    obsidian
    tetex
    neovim

    # various programming-related things
    firebase-tools
    git
    gh
    cmakeMinimal
    gnumake42

    # other misc stuff
    fluent-reader
    dhcpcd
		unzip
		wget
    firefox
    bc
    mc
    discord
    keepassxc
    eza
    vlc
    syncthing
    catppuccin-gtk
    pinentry-curses

    # programming langs + related stuff
		lua-language-server
    go
    sass
    gcc
    deno
    rustc
    cargo
    python3
    jdk
    zig
    nim
    html-tidy
    stylelint
    zls
		nodejs_21
    rust-analyzer # needed for rust in doom emacs
    shellcheck # needed for shell in doom emacs
    nixfmt # needed for nix in doom emacs
    isort # needed for python in doom emacs
    pipenv # needed for python in doom emacs
  ];

  services.xserver.windowManager.qtile.enable = true;
  
  programs.hyprland.enable = true;

  programs.fish.enable = true;

  programs.steam.enable = true;

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
