{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit nixpkgs compiler; };

in

  if pkgs.lib.inNixShell then drv.env else drv
