{
  description = "hwarden-agent";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/687f05a9184cad4eaf905c48b63649e3a86f5433";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      sourceRevision = self.shortRev or self.dirtyShortRev or self.rev or "unknown";
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hpkgs = import ./nix/haskell-packages.nix { inherit pkgs; };
        in
        rec {
          default = pkgs.callPackage ./nix/package.nix {
            inherit sourceRevision;
            haskellPackages = hpkgs;
          };
          hwarden-agent = default;
        });

      nixosModules.default =
        { lib, pkgs, ... }:
        let
          system = pkgs.stdenv.hostPlatform.system;
        in
        {
          imports = [ ./nix/module.nix ];

          services.hwarden-agent.package =
            lib.mkDefault self.packages.${system}.default;
        };
    };
}
