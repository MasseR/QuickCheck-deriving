with (import <nixpkgs> {});

let
  QuickCheck-deriving = haskellPackages.callPackage ./. {};

in

mkShell {
  buildInputs = [
    hlint
    ghcid
    stylish-haskell
    cabal2nix
    haskellPackages.cabal-install
    (haskellPackages.ghcWithPackages (_: QuickCheck-deriving.buildInputs ++ QuickCheck-deriving.propagatedBuildInputs))
  ];
}
