{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  combineOverrides = old: fold composeExtensions (old.overrides or (_: _: { }));

  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = combineOverrides old [
      (pkgs.haskell.lib.packageSourceOverrides {
        fraction-optic = ../fraction-optic;
      })
      (new: old: {
        integer-types = new.callPackage ./haskell/integer-types.nix {};
      })
    ];
  });

in {
  devShells.default = pkgs.mkShell {
    inputsFrom = [ haskellPackages.fraction-optic.env ];
    buildInputs = [ pkgs.haskell-language-server pkgs.cabal-install ];
  };
}
