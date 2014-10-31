let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.myHaskellPackages.override {
    extension = self: super: {
      squealer = pkgs.myHaskellPackages.callPackage ./. {};
    };
  };

in
  pkgs.lib.overrideDerivation haskellPackages.squealer (attrs: {
    buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
  })
