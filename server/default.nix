{ pkgs ? import ../nixpkgs.nix }:
let 
  hpkgs = import ../nix/hls.nix {};
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
        [ cabal-install hpkgs.hpkgs.haskell-language-server ];
     shellHook = ''
        export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
     '';
  };
}

