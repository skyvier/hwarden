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
  packageSrc = pkgs.lib.sourceByRegex ./. [
    "^app(/.*)?$"
    "^src(/.*)?$"
    "^test(/.*)?$"
    "^hwarden-agent\\.cabal$"
  ];
  hwardenAgent =
    (hpkgs.callCabal2nix "hwarden-agent" packageSrc {}).overrideAttrs (old: {
      preCheck =
        (old.preCheck or "")
        + ''
          agent_test_exe="$(find "$PWD" -type f -name hwarden-agent -path '*/build/hwarden-agent/hwarden-agent' | head -n 1)"
          if [ -z "$agent_test_exe" ]; then
            echo "failed to locate built hwarden-agent executable for tests" 1>&2
            exit 1
          fi
          export HWARDEN_AGENT_TEST_EXE="$agent_test_exe"
        '';
    });

  wrappedHwardenAgent = pkgs.symlinkJoin {
    name = "hwarden-agent-wrapped";
    paths = [ hwardenAgent ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram "$out/bin/hwarden-agent" \
        --set HWARDEN_BW_PATH "${bitwardenCli}/bin/bw" \
        --set HWARDEN_VERSION "${shortGitRevision}"
    '';
  };
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
    bitwardenCli
    wrappedHwardenAgent
  ];
  shellHook = ''
    export HWARDEN_BW_PATH="${bitwardenCli}/bin/bw"
    export PATH="${wrappedHwardenAgent}/bin:$PATH"
  '';
}
