{
  config,
  flake-inputs,
  ...
}:
{
  imports = [
    ../base.nix
    flake-inputs.home-manager.darwinModules.home-manager
    ../home-manager.nix
  ];

  config =
    let
      cfg = config.nix-config;
    in
    {
      nix = {
        # Let Determinate Nix handle Nix configuration.
        enable = false;
      };

      # Custom settings written to /etc/nix/nix.custom.conf
      determinate-nix.customSettings = {
        inherit (config.nix.settings) keep-derivations keep-outputs trusted-users;
      };

      system = {
        keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
          # NB: Setting swapLeftCommandAndLeftAlt affects all keyboards;
          # Lookup codes at: https://hidutil-generator.netlify.app
          userKeyMapping = [
            # Remap right option to right control.
            {
              HIDKeyboardModifierMappingSrc = 30064771302;
              HIDKeyboardModifierMappingDst = 30064771300;
            }
          ];
        };

        defaults = {
          dock = {
            autohide = true;
          };

          hitoolbox.AppleFnUsageType = "Do Nothing";

          # View current settings: defaults read -g
          NSGlobalDomain = {
            "com.apple.keyboard.fnState" = true;
            "com.apple.mouse.tapBehavior" = 1;

            InitialKeyRepeat = 15;
            KeyRepeat = 5;
          };

          screencapture = {
            location = "/Users/${cfg.username}/Pictures/Screenshots";
          };

          trackpad = {
            Clicking = true;
            Dragging = true;
          };

          WindowManager.EnableTiledWindowMargins = false;
        };

        primaryUser = cfg.username;
      };
    };
}
