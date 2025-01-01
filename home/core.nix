{
  lib,
  pkgs,
  ...
}:
{
  config = {
    home.packages = with pkgs; [
      du-dust
      duf
      hyperfine
      libtree
      procs
    ];

    home.file = {
      ".local/bin" = {
        source = local/bin;
        recursive = true;
      };
    };

    home.sessionPath = [
      "$HOME/.local/bin"
    ];

    programs = {
      bat = {
        enable = true;
        config = {
          theme = "Nord";
        };
      };

      bottom.enable = true;
      eza.enable = true;
      fd.enable = true;
      gpg.enable = true;
      lsd.enable = true;
      ripgrep.enable = true;

      direnv = {
        enable = true;

        nix-direnv = {
          enable = true;
        };
      };

      fzf =
        let
          fd = lib.getExe pkgs.fd;
        in
        {
          enable = true;
          changeDirWidgetCommand = "${fd} --type d";
          defaultCommand = "${fd} --type f";
          fileWidgetCommand = "${fd} --type f";
          tmux.enableShellIntegration = true;
        };
    };
  };
}
