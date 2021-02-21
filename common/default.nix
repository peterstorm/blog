{ src ? import ../nixpkgs.nix }:

with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/db5400ad7801076a8eac1c071a654b7317b78811.tar.gz";
  sha256 = "0ij4gw8ypnrdh7klscqczzycyhdnwzdcp83i9pxdbd8y9kmcgz4l";
}) {});

let
  # pkgs = src.pkgs;
  hls = src.hls;
  # hpkgs = import ../nix/hls.nix {};
  haskellPackages = pkgs.haskell.packages.ghc865;
  common = pkgs.haskellPackages.callCabal2nix "common" (src.gitignoreSource ./.) {};
  name = "blog";

in
  {
    my_project = common;
    shell = haskellPackages.shellFor {
      packages = p: [common];
      buildInputs = with haskellPackages;
        [ cabal-install hls.hpkgs.haskell-language-server stylish-haskell ]; 
          shellHook = ''
     export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
  '';
  };
}
