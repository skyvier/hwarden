let
  flake = import ../../flake.nix;
  nixpkgs = import <nixpkgs> {};
  system = builtins.currentSystem;

  self = outputs // {
    shortRev = "testrev";
  };

  outputs = flake.outputs {
    inherit self;
    nixpkgs = {
      inherit (nixpkgs) lib;
      legacyPackages.${system} = nixpkgs;
    };
  };

  evaluated = import <nixpkgs/nixos> {
    configuration = {
      imports = [ outputs.nixosModules.default ];

      boot.loader.grub.enable = false;
      fileSystems."/" = {
        device = "test";
        fsType = "ext4";
      };
      system.stateVersion = "25.05";

      services.hwarden-agent.enable = true;
    };
  };
in
assert outputs.packages.${system}.default.name == "hwarden-agent";
assert outputs.packages.${system}.hwarden-agent == outputs.packages.${system}.default;
assert builtins.elem "hwarden-agent.service" (builtins.attrNames evaluated.config.systemd.user.units);
true
