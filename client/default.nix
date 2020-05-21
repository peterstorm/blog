{ pkgs ? import ../nixpkgs.nix }:

let
  haskellPackages = pkgs.haskell.packages.ghc864;
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
      buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install haskellPackages.ghcid ];
  }) else client_pkg


