{
  config,
  ...
}:
let
  tmux-extra-conf = "tmux/extra.conf";
in
{
  config = {
    programs = {
      starship = {
        enable = true;
        # Nushell integration doesn't work in "dumb" terminals.
        enableNushellIntegration = false;
      };

      tmux = {
        enable = true;
        clock24 = true;
        extraConfig = "source-file ${config.xdg.configHome}/${tmux-extra-conf}";
        focusEvents = true;
        historyLimit = 8192;
        terminal = "screen-256color";
      };
    };

    xdg.configFile = {
      "alacritty/alacritty.yml".source = config/alacritty/alacritty.yml;

      "termite/config".source = config/termite/config;

      "psql/psqlrc".source = config/psql/psqlrc;

      "readline/inputrc".source = config/readline/inputrc;

      "${tmux-extra-conf}".source = config/tmux/conf;
    };

    home.sessionVariables = {
      INPUTRC = "${config.xdg.cacheHome}/readline/inputrc";

      PSQLRC = "${config.xdg.cacheHome}/psql/psqlrc";
      PSQL_HISTORY = "${config.xdg.dataHome}/psql/history";
    };
  };
}
