{
  config,
  flake-inputs,
  ...
}:
{
  imports = [
    flake-inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    verbose = true;

    # Install packages to /etc/profiles; needed to run `nixos-rebuild build-vm`.
    useUserPackages = true;

    # Use system-level Nixpkgs.
    useGlobalPkgs = true;

    users.${config.nix-config.user} =
      { ... }:
      {
        imports = [
          flake-inputs.nix-index-database.hmModules.nix-index
          ../home/default.nix
        ];
      };
  };
}
