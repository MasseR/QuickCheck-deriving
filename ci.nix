with (import <nixpkgs> {});

haskell.lib.failOnAllWarnings (haskellPackages.callPackage ./. {})
