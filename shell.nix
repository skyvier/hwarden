let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/687f05a9184cad4eaf905c48b63649e3a86f5433.tar.gz";
    sha256 = "1n1lqgnk84mf28v23f3q8z6xwc79biqwqqy84cg75mrrpa65k4mx";
  };

  pkgs = import nixpkgs {};
  hpkgs = pkgs.haskellPackages;
  sourceRevision = import ./nix/source-revision.nix {
    lib = pkgs.lib;
    root = ./.;
  };

  bitwardenCli = pkgs.bitwarden-cli;
  wrappedHwardenAgent = pkgs.callPackage ./nix/package.nix {
    bitwarden-cli = bitwardenCli;
    sourceRevision = sourceRevision;
  };
  hwardenAgent = wrappedHwardenAgent.unwrapped;
in
hpkgs.shellFor {
  packages = p: [ hwardenAgent ];
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.jq
    pkgs.netcat-openbsd
    pkgs.rofi
    pkgs.socat
    pkgs.xclip
    pkgs.ghcid
    pkgs.glow
    pkgs.entr
    bitwardenCli
    wrappedHwardenAgent
  ];
  shellHook = ''
    export HWARDEN_BW_PATH="${bitwardenCli}/bin/bw"
    export PATH="${wrappedHwardenAgent}/bin:$PATH"
  '';
}
