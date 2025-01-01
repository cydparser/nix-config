{
  config = {
    programs = {
      starship.enable = true;
    };

    xdg.configFile = {
      "alacritty/alacritty.yml".source = config/alacritty/alacritty.yml;

      "termite/config".source = config/termite/config;

      "psql/psqlrc".source = config/psql/psqlrc;

      "readline/inputrc".source = config/readline/inputrc;

      "tmux/conf".source = config/tmux/conf;
    };

    home.sessionVariables = {
      INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";

      PSQLRC = "$XDG_CONFIG_HOME/psql/psqlrc";
      PSQL_HISTORY = "$XDG_DATA_HOME/psql/history";
    };
  };
}
