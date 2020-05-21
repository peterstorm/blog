{ pkgs ? import ../nixpkgs.nix }:

let
  haskellPackages = pkgs.haskell.packages.ghc864;
  common = pkgs.haskellPackages.callCabal2nix "common" ./. {};

in
   if pkgs.lib.inNixShell then common.env.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install haskellPackages.ghcid ];
  }) else common

