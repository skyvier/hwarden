{ system ? builtins.currentSystem }:

let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  locked = lock.nodes.nixpkgs.locked;
  src = builtins.fetchTarball {
    url = "https://github.com/${locked.owner}/${locked.repo}/archive/${locked.rev}.tar.gz";
    sha256 = locked.narHash;
  };
in
{
  inherit locked src;
  rev = locked.rev;
  pkgs = import src { inherit system; };
}
