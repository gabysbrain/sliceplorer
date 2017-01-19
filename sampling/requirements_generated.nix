# generated using pypi2nix tool (version: 1.6.0)
#
# COMMAND:
#   pypi2nix -v -V 3.5 -r sampling/requirements.txt -E R -E readline -E zlib -E pcre -E bzip2 -E lzma -E icu -E libiconv
#

{ pkgs, python, commonBuildInputs ? [], commonDoCheck ? false }:

self: {

  "PyMonad" = python.mkDerivation {
    name = "PyMonad-1.3";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/6b/c5/f1affc732c35903266b164c26dea2fda56c8a2eb498d18bcd38349c66f5b/PyMonad-1.3.tar.gz"; sha256 = "a3a2621bb2175d4e1c710c52338991a63f22160640b1f34571a6f90f9a689764"; };
    doCheck = commonDoCheck;
    buildInputs = commonBuildInputs;
    propagatedBuildInputs = [ ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = "Copyright (c) 2014, Jason DeLaat";
      description = "Collection of classes for programming with functors, applicative functors and monads.";
    };
  };



  "rpy2" = python.mkDerivation {
    name = "rpy2-2.7.6";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/1c/b5/37859f3995b17db989cd5b65cef388e97e9bd34272209c4628a22461c995/rpy2-2.7.6.tar.gz"; sha256 = "ea35a2f37fd36e8af069731bf1421fbb2d3f8550bcb55fde3bebf0bcb1b00a5a"; };
    doCheck = commonDoCheck;
    buildInputs = commonBuildInputs;
    propagatedBuildInputs = [
      self."six"
    ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = licenses.gpl2Plus;
      description = "Python interface to the R language (embedded R)";
    };
  };



  "six" = python.mkDerivation {
    name = "six-1.10.0";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/b3/b2/238e2590826bfdd113244a40d9d3eb26918bd798fc187e2360a8367068db/six-1.10.0.tar.gz"; sha256 = "105f8d68616f8248e24bf0e9372ef04d3cc10104f1980f54d57b2ce73a5ad56a"; };
    doCheck = commonDoCheck;
    buildInputs = commonBuildInputs;
    propagatedBuildInputs = [ ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = licenses.mit;
      description = "Python 2 and 3 compatibility utilities";
    };
  };



  "sobol" = python.mkDerivation {
    name = "sobol-0.9";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/95/ae/3052500eb9e6d7aa1d661d011c48e0a71603ce89cd6fd99299d4fd7a6924/sobol-0.9.tar.gz"; sha256 = "47491d42391208e820452f54fa1f290dad0e26ba6b2f52facf8f08dbbc7ee5d5"; };
    doCheck = commonDoCheck;
    buildInputs = commonBuildInputs;
    propagatedBuildInputs = [ ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = licenses.mit;
      description = "SOBOL quasi random number sequence generator";
    };
  };



  "sobol-seq" = python.mkDerivation {
    name = "sobol-seq-0.1.2";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/89/7a/7b374fd1f100bfea2624190f3dd879029e1aabe70d34e279ef456d522717/sobol_seq-0.1.2.zip"; sha256 = "1b1c7a7666f7a96121107e6541b615376909b045aa1a63c3f9cbc3650b675de6"; };
    doCheck = commonDoCheck;
    buildInputs = commonBuildInputs;
    propagatedBuildInputs = [ ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = licenses.mit;
      description = "Sobol sequence generator";
    };
  };

}