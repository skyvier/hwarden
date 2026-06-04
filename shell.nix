let
  lockedNixpkgs = import ./nix/nixpkgs-from-flake-lock.nix {};
  pkgs = lockedNixpkgs.pkgs;
  hpkgs = import ./nix/haskell-packages.nix { inherit pkgs; };
  sourceRevision = import ./nix/source-revision.nix {
    lib = pkgs.lib;
    root = ./.;
  };

  bitwardenCli = pkgs.bitwarden-cli;
  wrappedHwardenAgent = pkgs.callPackage ./nix/package.nix {
    bitwarden-cli = bitwardenCli;
    sourceRevision = sourceRevision;
    haskellPackages = hpkgs;
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
