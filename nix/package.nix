{
  lib,
  haskellPackages,
  bitwarden-cli,
  makeWrapper,
  symlinkJoin,
  sourceRevision ? "unknown",
}:

let
  packageSrc = lib.sourceByRegex ../. [
    "^app(/.*)?$"
    "^src(/.*)?$"
    "^test(/.*)?$"
    "^hwarden-agent\\.cabal$"
  ];

  hwardenAgent =
    (haskellPackages.callCabal2nix "hwarden-agent" packageSrc {}).overrideAttrs (old: {
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
in
symlinkJoin {
  name = "hwarden-agent";
  paths = [ hwardenAgent ];
  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram "$out/bin/hwarden-agent" \
      --set HWARDEN_BW_PATH "${bitwarden-cli}/bin/bw" \
      --set HWARDEN_VERSION "${sourceRevision}"
  '';
  passthru = {
    unwrapped = hwardenAgent;
  };
}
