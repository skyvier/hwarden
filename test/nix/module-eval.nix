let
  evaluated = import <nixpkgs/nixos> {
    configuration = {
      imports = [ ../../nix/module.nix ];

      boot.loader.grub.enable = false;
      fileSystems."/" = {
        device = "test";
        fsType = "ext4";
      };
      system.stateVersion = "25.05";

      services.hwarden-agent = {
        enable = true;
        serverUrl = "https://vault.bitwarden.com";
        cacheRefreshIntervalSeconds = 30;
      };
    };
  };

  pkgs = evaluated.pkgs;
  lib = pkgs.lib;
  service = evaluated.config.systemd.user.services.hwarden-agent;
in
assert !(lib.hasAttrByPath [ "services" "hwarden-agent" "bitwardenCliPackage" ] evaluated.options);
assert !(lib.hasAttr "HWARDEN_BW_PATH" service.environment);
assert service.environment.HWARDEN_SERVER_URL == "https://vault.bitwarden.com";
assert service.environment.HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS == "30";
assert service.environment.BITWARDENCLI_APPDATA_DIR == "%S/hwarden-agent/bitwarden-cli";
assert lib.hasSuffix "/bin/hwarden-agent" service.serviceConfig.ExecStart;
assert service.serviceConfig.RuntimeDirectory == "hwarden";
assert service.serviceConfig.RuntimeDirectoryMode == "0700";
assert service.serviceConfig.StateDirectory == "hwarden-agent";
assert service.serviceConfig.StateDirectoryMode == "0700";
assert service.serviceConfig.ReadWritePaths == [ "%t/hwarden" "%S/hwarden-agent" ];
assert service.wantedBy == [ "default.target" ];
true
