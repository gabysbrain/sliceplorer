{ pkgs, python }:

self: super: {
  "flask"  = pkgs.python35Packages.flask;
  "numpy"  = pkgs.python35Packages.numpy;
  "scipy"  = pkgs.python35Packages.scipy;
  "pandas" = python.overrideDerivation pkgs.python35Packages.pandas (old: {
    checkPhase = ''
      echo 'no tests'
    '';
  });
}
