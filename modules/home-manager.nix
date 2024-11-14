{
  config,
  flake-inputs,
  ...
}:
let
  cfg = config.nix-config;

  inherit (cfg) username;
in
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

    users.${username} =
      { ... }:
      {
        imports = [
          flake-inputs.nix-index-database.hmModules.nix-index
          ../home/default.nix
        ];

        home = {
          stateVersion = cfg.stateVersion;
        };
      };
  };
}
