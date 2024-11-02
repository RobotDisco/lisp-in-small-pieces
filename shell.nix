{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.chez
    pkgs.chicken
    pkgs.gambit
    pkgs.racket
  ];
}
