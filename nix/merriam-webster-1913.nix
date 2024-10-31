{ pkgs }:
pkgs.stdenvNoCC.mkDerivation {
  name = "merriam-webster-1913";
  sourceRoot = ".";
  src = pkgs.fetchurl {
    url = "https://s3.amazonaws.com/jsomers/dictionary.zip";
    sha256 = "09y673q5v46ps72pnm1jz0jx2bcyfbsmzw3f2x9y9s99k3yf79ps";
  };
  nativeBuildInputs = [ pkgs.unzip ];
  installPhase = ''
    mkdir -p "$out/dic"
    tar -C "$out/dic" -xf dictionary/stardict-dictd-web1913-2.4.2.tar.bz2
  '';
}
