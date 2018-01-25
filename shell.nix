{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, stdenv }:
      mkDerivation {
        pname = "initial-final";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base lens ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.ghc822;

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
