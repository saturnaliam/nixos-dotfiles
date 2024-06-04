{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    (catppuccin-sddm.override {
      flavor = "mocha";
      font = "Noto Sans";
      fontSize = "9";
      loginBackground = false;
    })

    # various things to make using the wm easier
    grim
    slurp
    dunst
    picom
    polybar
    swappy
    rofi-wayland-unwrapped
    gtk3
    nitrogen
    xfce.xfce4-screenshooter

    # text editors + required stuff for them
    ripgrep
    libreoffice-fresh # this is more than a text editor but idc
    fd
    obsidian
    tetex
    vim
    neovim

    # various programming-related things
    emacs-gtk
    git
    gh
    cmake
    openssl.dev
    gnumake42

    # other misc stuff
    cdrtools
    fluent-reader
    syncthing
    dhcpcd
    unzip
    spotify
    wget
    firefox
    bc
    mc
    discord
    keepassxc
    eza
    vlc
    catppuccin-gtk
    pinentry-curses

    # programming langs + related stuff
    vim
    posix_man_pages
    stdmanpages
    lua-language-server
    go
    sass
    gcc
    deno
    libsodium
    rustc
    cargo
    python3
    libtool
    cmake-language-server
    jdk
    zig
    nim
    erlang
    html-tidy
    stylelint
    zls
    nodejs_22
    shfmt
    black
    hugo
    python311Packages.pyflakes
    python311Packages.nose
    python311Packages.pytest
    python311Packages.python-lsp-server
    nodePackages.js-beautify
    rust-analyzer # needed for rust in doom emacs
    shellcheck # needed for shell in doom emacs
    nixfmt-classic # TODO this will prolly get replaced
    isort # needed for python in doom emacs
    pipenv # needed for python in doom emacs
    gleam

    python311Packages.pip

    (hiPrio clang-tools.override {
      llvmPackages = llvmPackages_16;
      enableLibcxx = true;
    })

    (dmenu.override {
      patches = [
        (fetchpatch {
          url =
            "https://tools.suckless.org/dmenu/patches/numbers/dmenu-numbers-20220512-28fb3e2.diff";
          hash = "sha256-lg7CItn11YPEe7T7aPt1DBybZlnLjKQGC8J+OcY44Js=";
        })

        (fetchpatch {
          url =
            "https://tools.suckless.org/dmenu/patches/line-height/dmenu-lineheight-5.2.diff";
          hash = "sha256-QdY2T/hvFuQb4NAK7yfBgBrz7Ii7O7QmUv0BvVOdf00=";
        })
      ];
    })
  ];

  services.xserver.windowManager.qtile = {
    enable = true;
    backend = "x11";
    extraPackages = p: with p; [ qtile-extras ];
  };

  programs.hyprland.enable = true;

  programs.fish.enable = true;

  programs.steam.enable = true;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.direnv.silent = true;

  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  services.syncthing = {
    enable = true;
    user = "nixos";
    dataDir = "/home/lucia/";
    configDir = "/home/lucia/.config/syncthing";
  };
}
