{ src ? import ../nixpkgs.nix }:

let
  pkgs = src.pkgs;
  hls = src.hls;
  haskellPackages = pkgs.haskell.packages.ghc864;
  server = pkgs.haskell.packages.ghc.callCabal2nix "server" (src.gitignoreSource ./.) {
    common = pkgs.haskell.packages.ghc.callCabal2nix "common" (src.gitignoreSource ../common) {};
  };
  name = "blog";

in
  {
    my_project = server;
    shell = haskellPackages.shellFor {
      packages = p: [server];
      buildInputs = with haskellPackages;
        [ cabal-install hls.hpkgs.haskell-language-server stylish-haskell ];
     shellHook = ''
        export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
     '';
  };
}

