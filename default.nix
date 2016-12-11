{ mkDerivation, accelerate, base, newtype, sigym4-null, stdenv }:
mkDerivation {
  pname = "sigym4-null-accelerate";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ accelerate base newtype sigym4-null ];
  homepage = "https://github.com/meteogrid/sigym4-null-accelerate";
  description = "sigym4-null lifted to Accelerate Expressions";
  license = stdenv.lib.licenses.bsd3;
}
