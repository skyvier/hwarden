let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/50ab793786d9de88ee30ec4e4c24fb4236fc2674.tar.gz";
    sha256 = "1s2gr5rcyqvpr58vxdcb095mdhblij9bfzaximrva2243aal3dgx";
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
    pkgs.socat
    bitwardenCli
    wrappedHwardenAgent
  ];
  shellHook = ''
    export PATH="${wrappedHwardenAgent}/bin:$PATH"
  '';
}
