{ pkgs ? import ../nixpkgs.nix }:
let 
  # sources = import ../nix/sources.nix;
  #nixpkgs = import sources.nixpkgs {};
  #githubTarball = owner: repo: rev:
  #  builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };
  # ghcide = (import (githubTarball "cachix" "ghcide-nix" "master") {})."ghcide-ghc864";

  haskellPackages = pkgs.haskell.packages.ghc864;
  server = pkgs.haskell.packages.ghc.callCabal2nix "server" ./. {
  common = pkgs.haskell.packages.ghc.callCabal2nix "common" ../common {};
  };

in
  if pkgs.lib.inNixShell then server.env.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install haskellPackages.ghcid (import (builtins.fetchTarball
          "https://github.com/hercules-ci/ghcide-nix/tarball/master"
        ) {}).ghcide-ghc864 ];
  }) else server
