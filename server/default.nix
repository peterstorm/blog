{ pkgs ? import ../nixpkgs.nix }:

let
  haskellPackages = pkgs.haskell.packages.ghc864;
  server = pkgs.haskell.packages.ghc.callCabal2nix "server" ./. {
    common = pkgs.haskell.packages.ghc.callCabal2nix "common" ../common {};
  };

in
  if pkgs.lib.inNixShell then server.env.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install haskellPackages.ghcid ];
  }) else server
