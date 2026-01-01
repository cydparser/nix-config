default:
    @just --list --justfile {{ justfile() }}

os-rebuild := if os() == "linux" { "nixos-rebuild" } else { "darwin-rebuild" }

switch:
    sudo {{ os-rebuild }} switch --flake . -L
    nix profile diff-closures --profile /nix/var/nix/profiles/system
