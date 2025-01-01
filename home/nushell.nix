inputs@{ ... }:
let
  utils = import ./utils.nix inputs;
in
{
  config = {
    programs = {
      nushell = {
        enable = true;
        shellAliases = utils.shellAliases // {
          "l" = "ls -a";
        };

        environmentVariables = {
          NU_USE_IR = "true";
        };
      };
    };
  };
}
