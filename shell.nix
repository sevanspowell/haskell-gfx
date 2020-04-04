{}:

let
  pkgs = import (fetchGit (import ./nixpkgs.nix)) { };

  drv =
    pkgs.haskell.lib.addBuildTools
      (pkgs.haskellPackages.callCabal2nix "haskell-gfx" ./. {})
      (with pkgs.haskellPackages; [ ghcid cabal-install ] ++ (with pkgs; [ ]));
in
  drv.env
