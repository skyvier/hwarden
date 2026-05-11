{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps:
    with ps; [
      aeson
      bytestring
      directory
      filepath
      network
      process
      text
      unix
    ]);
in
pkgs.mkShell {
  packages = [
    ghc
    pkgs.cabal-install
    pkgs.socat
    pkgs.bitwarden-cli
  ];
}
