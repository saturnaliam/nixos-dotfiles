{
description = "System conf";
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  home-manager.url = "github:nix-community/home-manager/master";
  home-manager.inputs.nixpkgs.follows = "nixpkgs";

  tmux-nvim-src.url = "github:aserowy/tmux.nvim";
  tmux-nvim-src.flake = false;

  catppuccin-sddm-src.url = "github:catppuccin/sddm";
  catppuccin-sddm-src.flake = false;
};
outputs = {
  self,
  nixpkgs,
  home-manager,
...}@inputs:
  let
    moduleArgs = inputs;
    lib = nixpkgs.lib;
    username = "lucia";
  in {
    nixosConfigurations.nixos = lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./config
        ./hardware.nix
        { _module.args = moduleArgs; }
        home-manager.nixosModules.home-manager
        {
          home-manager.users.${username} = import ./home-manager;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = moduleArgs;
        }
      ];
    };
  };
}    
