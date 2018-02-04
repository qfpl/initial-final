{ mkDerivation, base, criterion, lens, stdenv }:
mkDerivation {
  pname = "vanilla";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens ];
  benchmarkHaskellDepends = [ base criterion ];
  license = stdenv.lib.licenses.bsd3;
}
