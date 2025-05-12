{
  imports = [
    ../users/cyd.nix
  ];

  networking.hostName = "moon";

  nix-config = {
    stateVersion = 6;
  };
}
