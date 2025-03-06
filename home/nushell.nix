inputs@{
  config,
  lib,
  ...
}:
let
  utils = import ./utils.nix inputs;
in
{
  config =
    let
      inherit (lib) modules;
    in
    modules.mkMerge [
      {
        programs = {
          nushell = {
            enable = true;
            shellAliases = utils.shellAliases // {
              "l" = "ls -a";
            };

            environmentVariables = {
              NU_USE_IR = "true";
            };
          };
        };
      }
      (modules.mkIf config.programs.starship.enable (
        let
          starship = config.programs.starship.package;

          starshipNu = "\"${config.xdg.cacheHome}/nushell/starship-${starship.version}.nu\"";
        in
        {
          programs.nushell = {
            extraEnv = ''
              if not (${starshipNu} | path exists) {
                mkdir (${starshipNu} | path dirname)
                ${lib.getExe starship} init nu | save --force ${starshipNu}
              }
            '';

            extraConfig = ''
              if $env.TERM != "dumb" {
                use ${starshipNu}
              }
            '';
          };
        }
      ))
    ];
}
