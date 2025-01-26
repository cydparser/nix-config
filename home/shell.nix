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

        historyControl = [ "ignoreboth" ];

        historyFile = "${config.xdg.dataHome}/bash/history";

        initExtra = ''
          mkdir -p "${builtins.dirOf config.programs.bash.historyFile}"
        '';

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
