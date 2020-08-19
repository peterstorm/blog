{ pkgs ? import ../nixpkgs.nix }:

let
  haskellPackages = pkgs.haskell.packages.ghc864;
  common = pkgs.haskellPackages.callCabal2nix "common" ./. {};
  name = "blog";

in
  {
    my_project = common;
    shell = haskellPackages.shellFor {
      packages = p: [common];
      buildInputs = with haskellPackages;
        [ cabal-install (import (builtins.fetchTarball 
          "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc864 ];
          shellHook = ''
     export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
  '';
  };
}
