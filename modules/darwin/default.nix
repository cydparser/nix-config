{
  config,
  ...
}:
{
  imports = [
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
          swapLeftCommandAndLeftAlt = true;
        };
      };
    };
}
