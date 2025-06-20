{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    home.packages =
      with pkgs;
      [
        du-dust
        duf
        hyperfine
        procs
      ]
      ++ lib.optional (!pkgs.stdenv.isDarwin) libtree;

    home.file =
      {
        ".local/bin" = {
          source = local/bin;
          recursive = true;
        };
      }
      // lib.optionalAttrs pkgs.stdenv.isDarwin {
        "Pictures/Screenshots/.keep".text = "";
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

      gpg = {
        enable = true;
        homedir = "${config.xdg.dataHome}/gnupg";
        settings = {
        };
      };

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

    services = {
      gpg-agent =
        let
          ttl = 9 * 60 * 60;
        in
        {
          enable = true;
          defaultCacheTtl = ttl;
          defaultCacheTtlSsh = ttl;
          maxCacheTtl = ttl;
          maxCacheTtlSsh = ttl;
        };
    };
  };
}
