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

    programs = {
      bat.enable = true;
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
