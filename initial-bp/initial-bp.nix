{ mkDerivation, base, stdenv, lens
}:
mkDerivation {
  pname = "initial-bp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens
  ];
  license = stdenv.lib.licenses.bsd3;
}
