{ pkgs, ... }:
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
      bottom = true;
      eza.enable = true;
      fd.enable = true;
      lsd.enable = true;
      ripgrep.enable = true;
    };
  };
}
