{ pkgs }:

pkgs.haskellPackages.override {
  overrides = hself: hsuper: {
    symparsec = hsuper.symparsec_2_0_0;
  };
}
