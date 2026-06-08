{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.hwarden-agent;
  sourceRevision = import ./source-revision.nix { inherit lib; };

  defaultPackage = pkgs.callPackage ./package.nix {
    inherit sourceRevision;
  };

  environment = {
    HWARDEN_SERVER_URL = cfg.serverUrl;
    HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS =
      toString cfg.cacheRefreshIntervalSeconds;
    BITWARDENCLI_APPDATA_DIR = "%S/hwarden-agent/bitwarden-cli";
  } // cfg.extraEnvironment;
in
{
  options.services.hwarden-agent = {
    enable = lib.mkEnableOption "hwarden-agent user service";

    package = lib.mkOption {
      type = lib.types.package;
      default = defaultPackage;
      defaultText = lib.literalExpression ''
        pkgs.callPackage ./nix/package.nix {
          sourceRevision = import ./nix/source-revision.nix { inherit lib; };
        }
      '';
      description = ''
        hwarden-agent package to run. The default package builds this
        repository and wraps the executable with the package's Bitwarden CLI.
      '';
    };

    serverUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://vault.bitwarden.eu";
      description = ''
        Bitwarden server URL configured in the agent's isolated CLI profile at
        startup.
      '';
    };

    cacheRefreshIntervalSeconds = lib.mkOption {
      type = lib.types.ints.positive;
      default = 60;
      description = ''
        Interval, in seconds, for refreshing the in-memory item cache after
        unlock.
      '';
    };

    extraEnvironment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = ''
        Extra environment variables for the hwarden-agent user service.
        Do not use this for HWARDEN_SERVER_URL or
        HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS; use the dedicated options
        instead.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.hwarden-agent = {
      description = "hwarden-agent Bitwarden session daemon";
      documentation = [ "https://github.com/skyvier/hwarden-agent" ];
      wantedBy = [ "default.target" ];
      environment = environment;

      serviceConfig = {
        ExecStart = "${cfg.package}/bin/hwarden-agent";
        Restart = "on-failure";
        RestartSec = "5s";
        RuntimeDirectory = "hwarden";
        RuntimeDirectoryMode = "0700";
        StateDirectory = "hwarden-agent";
        StateDirectoryMode = "0700";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = "read-only";
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ "%t/hwarden" "%S/hwarden-agent" ];
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SystemCallArchitectures = "native";
        UMask = "0077";
      };
    };
  };
}
