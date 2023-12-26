{
  outputs = {...}: {
    nixosModules = {
      configuration = ./configuration.nix;
      tpad = ./host/tpad.nix;
    };
  };
}
