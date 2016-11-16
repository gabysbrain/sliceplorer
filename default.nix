with import <nixpkgs> {}; 

let
  mypypi = import ./mypypi.nix {
    pkgs = pkgs;
    python = python35Packages;
  };

in
{
  devEnv = stdenv.mkDerivation {
    name = "sliceplorer-dev";
    buildInputs = [
      pkgconfig freetype
      nodejs
      haskellPackages.purescript

      pypi2nix
      npm2nix
      nodePackages.bower2nix

      python3
      python3Packages.matplotlib
      python3Packages.pandas
      python3Packages.numpy
      python3Packages.scipy
      python3Packages.flask
      mypypi.rpy2
      mypypi.sobol-seq
    ];
    
    bowerComponents = pkgs.buildBowerComponents {
      name = "sliceplorer";
      generated = ./bower-packages-generated.nix;
      src = ./slice-viewer;
    };
  };
}

