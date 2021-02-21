{ src ? import ../nixpkgs.nix }:

with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/db5400ad7801076a8eac1c071a654b7317b78811.tar.gz";
  sha256 = "0ij4gw8ypnrdh7klscqczzycyhdnwzdcp83i9pxdbd8y9kmcgz4l";
}) {});

let
  # pkgs = src.pkgs;
  haskellPackages = pkgs.haskell.packages.ghc865;
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "client" ./. {
  common = pkgs.haskell.packages.ghcjs.callCabal2nix "common" ../common {};
  };

  client_pkg = pkgs.stdenv.mkDerivation {
    name = "client";
    src = ./.;
    installPhase = ''
      mkdir -p $out/static
      ${pkgs.closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
    '';
  };

in
  if pkgs.lib.inNixShell then client.env.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install ];
  }) else client_pkg
