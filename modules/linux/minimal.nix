{ flake-inputs, ... }:
{
  imports = [
    ../base.nix
    flake-inputs.home-manager.nixosModules.home-manager
    ../home-manager.nix
  ];

  config = {
    system.nixos-init.enable = true;
  };
}
