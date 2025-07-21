inputs@{
  config,
  iterm2-color-schemes,
  ...
}:
let
  tmux-extra-conf = "tmux/extra.conf";

  utils = import ./utils.nix inputs;

  isWsl = utils.isWsl;
in
{
  config = {
    programs = {
      rio = {
        enable = !isWsl;

        settings = {
          editor.program = "${config.programs.emacs.package}/bin/emacsclient";

          fonts = {
            family = "IosevkaTerm Nerd Font";
            # hinting = true;
            # size = 18;
            # features = [];
            # use-drawable-chars = true;
            # symbol-map = [];
            # disable-warnings-not-found = false;
            # additional-dirs = [];

            # regular = {
            # family = "cascadiacode";
            # style = "Normal";
            # width = "Normal";
            # weight = 400;
            # };

            # bold = {
            # family = "cascadiacode";
            # style = "Normal";
            # width = "Normal";
            # weight = 800;
            # };

            # italic = {
            # family = "cascadiacode";
            # style = "Italic";
            # width = "Normal";
            # weight = 400;
            # };

            # bold-italic = {
            # family = "cascadiacode";
            # style = "Italic";
            # width = "Normal";
            # weight = 800;
            # };
          };

          # hide-mouse-cursor-when-typing = false;

          navigation = {
            mode = "Plain";
            use-split = false;
          };

          theme = "jellybeans";
        };
      };

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
        mouse = true;
        prefix = "C-c";
        secureSocket = true;
        terminal = "screen-256color";
      };
    };

    xdg.configFile = {
      "alacritty/alacritty.yml".source = config/alacritty/alacritty.yml;

      "termite/config".source = config/termite/config;

      "psql/psqlrc".source = config/psql/psqlrc;

      "readline/inputrc".source = config/readline/inputrc;

      "rio/themes".source = "${iterm2-color-schemes}/rio";

      "${tmux-extra-conf}".source = config/tmux/conf;
    };

    home.sessionVariables = {
      INPUTRC = "${config.xdg.cacheHome}/readline/inputrc";

      PSQLRC = "${config.xdg.cacheHome}/psql/psqlrc";
      PSQL_HISTORY = "${config.xdg.dataHome}/psql/history";
    };
  };
}
