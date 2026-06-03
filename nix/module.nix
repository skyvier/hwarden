{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.hwarden-agent;

  defaultPackage = pkgs.callPackage ./package.nix {
    bitwarden-cli = cfg.bitwardenCliPackage;
    sourceRevision = "nixos-module";
  };

  environment = {
    HWARDEN_BW_PATH = "${cfg.bitwardenCliPackage}/bin/bw";
    HWARDEN_SERVER_URL = cfg.serverUrl;
    HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS =
      toString cfg.cacheRefreshIntervalSeconds;
  } // cfg.extraEnvironment;
in
{
  options.services.hwarden-agent = {
    enable = lib.mkEnableOption "hwarden-agent user service";

    package = lib.mkOption {
      type = lib.types.package;
      default = defaultPackage;
      defaultText = lib.literalExpression "pkgs.callPackage ./nix/package.nix {}";
      description = ''
        hwarden-agent package to run. The default package builds this
        repository and wraps the executable with the selected Bitwarden CLI.
      '';
    };

    bitwardenCliPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.bitwarden-cli;
      defaultText = lib.literalExpression "pkgs.bitwarden-cli";
      description = ''
        Bitwarden CLI package used by the service. Its bw executable path is
        passed to hwarden-agent through HWARDEN_BW_PATH.
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
        Do not use this for HWARDEN_BW_PATH, HWARDEN_SERVER_URL, or
        HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS; use the dedicated options
        instead.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.hwarden-agent = {
      Unit = {
        Description = "hwarden-agent Bitwarden session daemon";
        Documentation = [ "https://github.com/skyvier/hwarden-agent" ];
      };

      Service = {
        ExecStart = "${cfg.package}/bin/hwarden-agent";
        Environment = lib.mapAttrsToList (name: value: "${name}=${value}") environment;
        Restart = "on-failure";
        RestartSec = "5s";
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
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SystemCallArchitectures = "native";
        UMask = "0077";
      };

      Install.WantedBy = [ "default.target" ];
    };
  };
}
