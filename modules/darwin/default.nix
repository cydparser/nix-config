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
      system = {
        keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
          # NB: Setting swapLeftCommandAndLeftAlt affects all keyboards;
          userKeyMapping = [
            # Remap right command to right control.
            {
              HIDKeyboardModifierMappingSrc = 30064771303;
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
      };
    };
}
