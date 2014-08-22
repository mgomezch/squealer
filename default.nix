{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
with haskellPackages; cabal.mkDerivation (self: {
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
