{ mkDerivation, base, bifunctors, containers, directory, hpython
, lens, optparse-applicative, stdenv, text, validation
}:
mkDerivation {
  pname = "reindent";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors containers directory hpython lens
    optparse-applicative text validation
  ];
  executableHaskellDepends = [ base hpython text validation ];
  license = stdenv.lib.licenses.bsd3;
}
