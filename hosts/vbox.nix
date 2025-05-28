{
  imports = [
    vbox/hardware-configuration.nix
    ../users/cyd.nix
  ];

  boot = {
    loader = {
      grub = {
        enable = true;
        device = "/dev/sda";
        enableCryptodisk = true;
        useOSProber = true;
      };
    };

    initrd = {
      luks = {
        devices = {
          "luks-6f30a4b6-a51b-4f41-a742-7904b85c822b".device =
            "/dev/disk/by-uuid/6f30a4b6-a51b-4f41-a742-7904b85c822b";
          "luks-11983dfb-714f-4990-9656-6cc6315d88a9".keyFile = "/boot/crypto_keyfile.bin";
          "luks-6f30a4b6-a51b-4f41-a742-7904b85c822b".keyFile = "/boot/crypto_keyfile.bin";
        };
      };

      secrets = {
        "/boot/crypto_keyfile.bin" = null;
      };
    };
  };

  networking.hostName = "vbox";

  nix-config = {
    home-manager.stateVersion = "24.11";
  };

  system.stateVersion = "24.11";
}
