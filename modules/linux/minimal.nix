{ flake-inputs, ... }:{
  imports = [
    ../base.nix
    flake-inputs.home-manager.nixosModules.home-manager
    ../home-manager.nix
  ];
}
