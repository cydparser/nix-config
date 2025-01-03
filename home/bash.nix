inputs@{
  config,
  ...
}:
let
  utils = import ./utils.nix inputs;
in
{
  config = {
    programs = {
      bash = {
        enable = true;

        historyFile = "${config.xdg.dataHome}/bash/history";

        shellAliases = utils.shellAliases // {
        };
      };
    };
  };
}
