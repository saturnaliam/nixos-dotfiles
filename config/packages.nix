{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    neofetch
		unzip
		wget
    firefox
    tmux
    grim
    slurp
    swappy
    discord
    keepassxc
    eza
    rofi-wayland-unwrapped
    gnupg
    ripgrep
    fd

    # programming langs + related stuff
		lua-language-server
    swiftPackages.clang
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
