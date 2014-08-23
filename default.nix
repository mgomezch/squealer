# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, baseUnicodeSymbols, cmdargs, lens, newtype
, shakespeare, shellEscape, text, utilityHt, yaml
}:

cabal.mkDerivation (self: {
  pname = "squealer";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson baseUnicodeSymbols cmdargs lens newtype shakespeare
    shellEscape text utilityHt yaml
  ];
  meta = {
    description = "PostgreSQL DDL generator for auditable databases";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
