{ config, pkgs, ... } : {

  fileSystems."/var/lib/docker" = {
    device = "/dev/disk/by-uuid/a9e31858-a572-48d2-b9d1-85f73b723066";
    fsType = "ext4";
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "devicemapper";
  };

}
