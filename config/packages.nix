{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # various things to make using the wm easier
    grim   
    slurp 
    swappy
    rofi-wayland-unwrapped
    gtk3

    # text editors + required stuff for them
    helix
    ripgrep
    libreoffice-fresh # this is more than a text editor but idc
    fd
    obsidian
    neovim

    # various programming-related things
    firebase-tools
    git
    gh
    cmakeMinimal
    gnumake42

    # other misc stuff
    dhcpcd
		unzip
		wget
    firefox
#    tmux
    mc
    discord
    keepassxc
    eza
    vlc
    syncthing
    pinentry-curses

    # programming langs + related stuff
		lua-language-server
    llvmPackages_9.clang-unwrapped
    go
    deno
    rustc
    cargo
    python3
    jdk
		nodejs_21
    rust-analyzer # needed for rust in doom emacs
    shellcheck # needed for shell in doom emacs
    nixfmt # needed for nix in doom emacs
    isort # needed for python in doom emacs
    pipenv # needed for python in doom emacs
  ];

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
