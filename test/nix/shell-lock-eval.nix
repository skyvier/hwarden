let
  lockedNixpkgs = import ../../nix/nixpkgs-from-flake-lock.nix {};
  lock = builtins.fromJSON (builtins.readFile ../../flake.lock);
  lockedNixpkgsRev = lock.nodes.nixpkgs.locked.rev;
  shellSource = builtins.readFile ../../shell.nix;
in
assert lockedNixpkgs.rev == lockedNixpkgsRev;
assert builtins.match ".*nixpkgs-from-flake-lock\\.nix.*" shellSource != null;
true
