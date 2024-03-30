{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    helix
    dhcpcd
    neofetch
		unzip
		wget
    firefox
    tmux
    grim
    slurp
    appimage-run
    swappy
    discord
    keepassxc
    eza
    rofi-wayland-unwrapped
    ripgrep
    fd
    pinentry-curses
    libreoffice-fresh
    obsidian

    # programming langs + related stuff
		lua-language-server
    libclang
    go
    deno
    rustc
    cargo
    python3
    jdk
    gnumake42
    cmakeMinimal
    neovim
    git
    gh
		nodejs_21
    rust-analyzer # needed for rust in doom emacs
    shellcheck # needed for shell in doom emacs
    nixfmt # needed for nix in doom emacs
    isort # needed for python in doom emacs
    pipenv # needed for python in doom emacs
    glow
  
    (catppuccin-gtk.override {
      accents = [ "pink" ];
      size = "compact";
      tweaks = [ "rimless" "black" ];
      variant = "mocha";
    })
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
