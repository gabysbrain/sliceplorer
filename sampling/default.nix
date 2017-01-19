{}:

let
  pkgs = import <nixpkgs> {};
  python = import ./requirements.nix { inherit pkgs; };
in python.mkDerivation {
  name = "sliceplorer-server-0.1.0";
  src = if pkgs.lib.inNixShell then null else ./.;
  propagatedBuildInputs = builtins.attrValues python.packages;
}
