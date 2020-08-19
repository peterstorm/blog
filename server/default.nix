{ pkgs ? import ../nixpkgs.nix }:
let 
  # sources = import ../nix/sources.nix;
  #nixpkgs = import sources.nixpkgs {};
  #githubTarball = owner: repo: rev:
  #  builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };
  # ghcide = (import (githubTarball "cachix" "ghcide-nix" "master") {})."ghcide-ghc864";
  name = "blog";

  haskellPackages = pkgs.haskell.packages.ghc864;
  server = pkgs.haskell.packages.ghc.callCabal2nix "server" ./. {
  common = pkgs.haskell.packages.ghc.callCabal2nix "common" ../common {};
  };

in
  {
    my_project = server;
    shell = haskellPackages.shellFor {
      packages = p: [server];
      buildInputs = with haskellPackages;
        [ cabal-install (import (builtins.fetchTarball 
          "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc864 ];
          shellHook = ''
     export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
  '';
  };
}

  #if pkgs.lib.inNixShell then server.env.overrideAttrs (old: {
   #   buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install (import (builtins.fetchTarball
    #      "https://github.com/hercules-ci/ghcide-nix/tarball/master"
     #   ) {}).ghcide-ghc864 ];
 # }) else server
