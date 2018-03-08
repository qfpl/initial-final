{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage (with haskellPackages;
    mkDerivation {
      pname = "initial-final";
      version = "0.1.0.0";
      src = ./.;
      libraryHaskellDepends = [ base lens ];
      benchmarkHaskellDepends = [ base criterion ];
      license = pkgs.stdenv.lib.licenses.bsd3;
    }
  ) {};
in
  if pkgs.lib.inNixShell then drv.env else drv

