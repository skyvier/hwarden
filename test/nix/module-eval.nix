let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  module = import ../../nix/module.nix;
  evaluated = lib.evalModules {
  modules = [
    {
      config._module.args.pkgs = pkgs;
      options.systemd.user.services = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = {};
      };
    }
    module
    {
      services.hwarden-agent = {
        enable = true;
        serverUrl = "https://vault.bitwarden.com";
        cacheRefreshIntervalSeconds = 30;
      };
    }
  ];
  };

  service = evaluated.config.systemd.user.services.hwarden-agent;
  env = service.Service.Environment;
in
assert lib.elem "HWARDEN_BW_PATH=${pkgs.bitwarden-cli}/bin/bw" env;
assert lib.elem "HWARDEN_SERVER_URL=https://vault.bitwarden.com" env;
assert lib.elem "HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS=30" env;
assert service.Install.WantedBy == [ "default.target" ];
true
