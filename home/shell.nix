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

      zsh = {
        enable = false;

        history = {
          extended = true;
          ignoreDups = true;
          ignoreSpace = true;
          path = "${config.xdg.dataHome}/zsh/history";
          share = true;
        };

        shellAliases = utils.shellAliases;

        syntaxHighlighting.enable = true;
      };
    };
  };
}
