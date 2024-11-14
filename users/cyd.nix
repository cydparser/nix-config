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

  users.users.${username} = {
    isNormalUser = true;
  };
}
