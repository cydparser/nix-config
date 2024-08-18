{ ... }:
{
  config = {
    programs = {
      nushell = {
        enable = true;
        shellAliases = {
          "l" = "ls -a";
          "rusti" = "evcxr";
        };

        environmentVariables = {
          NU_USE_IR = "true";
        };
      };
    };
  };
}
