{ lib, ... }:
rec {
  mkEnable = name: lib.mkEnableOption name // { default = true; };

  isWsl = osConfig: builtins.hasAttr "wsl" osConfig && osConfig.wsl.enable;

  # TODO: This is very brittle.
  isWayland =
    osConfig:
    isWsl osConfig
    || !(
      builtins.hasAttr "services" osConfig
      && builtins.hasAttr "xserver" osConfig.services
      && osConfig.services.xserver.windowManager.xmonad.enable == true
    );
}
