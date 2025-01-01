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

  shellAliases = {
    dkc = "docker compose";

    em = "emacsclient -n";
    et = "emacsclient -t";

    l = "eza -la --sort newest";
    lt = "eza --tree --icons";
    lt2 = "eza --tree --icons --level 2";

    rusti = "evcxr";
  };
}
