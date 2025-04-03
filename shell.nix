
# nix-shell env for oreo
# this is for testing on crane only

let
  pkgs = import <nixpkgs> {};
  pythonPkgs = pkgs.python313.withPackages (ps: with ps; [
    numpy
  ]);

in pkgs.mkShell rec {
  name = "myPython";
  buildInputs = with pkgs; [ pythonPkgs ];
  # shellHook = ''
  #   export PS1='\n\[\033[1;34m\][${name}@\h:\w]\$\[\033[0m\] '
  # '';
}

