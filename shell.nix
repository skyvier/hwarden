let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/687f05a9184cad4eaf905c48b63649e3a86f5433.tar.gz";
    sha256 = "1n1lqgnk84mf28v23f3q8z6xwc79biqwqqy84cg75mrrpa65k4mx";
  };

  pkgs = import nixpkgs {};
  hpkgs = pkgs.haskellPackages;
  stripTrailingNewline = pkgs.lib.removeSuffix "\n";
  gitHead =
    if builtins.pathExists (./.git + "/HEAD") then
      stripTrailingNewline (builtins.readFile (./.git + "/HEAD"))
    else
      null;
  gitRevision =
    if gitHead == null then
      "unknown"
    else if pkgs.lib.hasPrefix "ref: " gitHead then
      let
        gitRefPath = ./.git + "/${pkgs.lib.removePrefix "ref: " gitHead}";
      in
      if builtins.pathExists gitRefPath then
        stripTrailingNewline (builtins.readFile gitRefPath)
      else
        "unknown"
    else
      gitHead;
  shortGitRevision =
    if gitRevision == "unknown" then
      gitRevision
    else
      builtins.substring 0 7 gitRevision;

  bitwardenCli = pkgs.bitwarden-cli;
  wrappedHwardenAgent = pkgs.callPackage ./nix/package.nix {
    bitwarden-cli = bitwardenCli;
    sourceRevision = shortGitRevision;
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
