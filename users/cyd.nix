{ ... }:
let
  username = "cyd";
in
{
  home-manager.users.${username} = {
    nix-config = {
      git.user = {
        name = "cydparser";
        email = "cydparser@gmail.com";
      };
    };
  };

  nix-config = {
    inherit username;
  };

  time.timeZone = "America/Los_Angeles";

  users.users.${username} = {
    isNormalUser = true;

    extraGroups = [
      "audio"
      "cdrom"
      "docker"
      "floppy"
      "input"
      "libvirtd"
      "lp"
      "systemd-journal"
      "vboxsf"
      "video"
      "wheel"
    ];

    uid = 1000;
  };
}
