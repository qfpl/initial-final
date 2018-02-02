{ mkDerivation, base, criterion, final, final-bp, initial
, initial-bp, stdenv
}:
mkDerivation {
  pname = "initial-final-bench";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base criterion final final-bp initial initial-bp
  ];
  license = stdenv.lib.licenses.bsd3;
}
