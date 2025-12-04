inputs@{
  config,
  lib,
  osConfig ? { },
  pkgs,
  ...
}:
let
  utils = import ./utils.nix inputs;
in
{
  options.nix-config.git =
    let
      inherit (lib) mkOption types;
    in
    {
      enable = utils.mkEnable "git";

      difftastic.enable = utils.mkEnable "difftastic";

      user = {
        name = mkOption { type = types.str; };
        email = mkOption { type = types.str; };
      };
    };

  config =
    let
      inherit (lib) mkIf modules;
      cfg = config.nix-config.git;
    in
    mkIf cfg.enable (
      modules.mkMerge [
        {
          home.packages = with pkgs; [ git-filter-repo ];

          programs.git = {
            enable = cfg.enable;

            settings = {
              user = {
                name = cfg.user.name;
                email = cfg.user.email;
              };

              aliases = rec {
                # Branch
                b = "branch";
                feature = "!git checkout -b $1 && git push origin -u";
                # Checkout
                co = "checkout";
                track = "!f() { local b=$1; local remote=$${2:-origin}; git checkout --track -b $b $remote/$b; }; f";
                # Commit
                cm = "commit -m";
                # Diff
                d = "diff";
                dc = "diff --cached";
                dn = "diff --name-only";
                dp = "!git diff --no-merges @{1}..";
                dt = "difftool";
                dw = "diff --word-diff";
                # Log
                l = "log --oneline --decorate --simplify-merges";
                lb = "log --pretty=format:'%ad %C(yellow)%h %Cred%an%Cblue%d %Creset%s' --date=short";
                lg = "log --graph --full-history --all --color --pretty=format:\"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s\"";
                lp = "${lb} --no-merges @{1}..";
                lf = "log --all --";
                # Rebase
                r = "rebase";
                ri = "rebase -i";
                ra = "rebase --abort";
                rc = "rebase --continue";
                # Revert
                rollback = "reset HEAD^";
                unrm = "checkout HEAD";
                unstage = "reset";
                # Stash
                s = "status -sb";
                sd = "stash drop";
                sl = "stash list";
                sp = "stash pop";
                ss = "stash save";
                sspp = "!git stash save && git pull && git stash pop";
                # Submodules
                smdiff = "!git diff && git submodule foreach 'git diff'";
                smpush = "push --recurse-submodules=on-demand";
                smrebase = "submodule update --remote --rebase";
              };
              delta = {
                enable = true;
                options = {
                  navigate = true;
                  side-by-side = true;
                  syntax-themes = "Visual Studio Dark+";
                };
              };

              extraConfig = {
                apply.whitespace = "fix";

                color.ui = true;

                branch.autosetuprebase = "always";

                diff = {
                  colorMoved = "dimmed-zebra";
                  submodule = "log";
                };

                init.defaultBranch = "main";

                log.date = "local";

                merge.conflictstyle = "diff3";

                push.default = "tracking";

                tag.sort = "version:refname";

                magit.hideCampaign = true;
              };
            };

            lfs = {
              enable = true;
              skipSmudge = true;
            };

            ignores = [
              ".DS_Store"
              ".devenv"
              ".dir-locals2.el"
              ".direnv"
              "nohup.out"
              "result"
              "tmp"
            ];
          };

          programs.delta = {
            enable = true;
            enableGitIntegration = true;
          };
        }
        (mkIf utils.isWsl {
          programs.git = {
            extraConfig = {
              credential.helper = "/mnt/c//Program\\ Files/Git/mingw64/bin/git-credential-manager.exe";
            };
          };
        })
        (mkIf cfg.difftastic.enable {
          programs = {
            git.settings = {
              extraConfig = {
                diff.tool = "difftastic";

                difftool.prompt = false;

                "difftool \"difftastic\"".cmd = "difft \"$LOCAL\" \"$REMOTE\"";

                pager.difftool = true;
              };
            };

            difftastic.enable = true;
          };
        })
      ]
    );
}
