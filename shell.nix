{ pkgs ? import <nixpkgs> { } }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.split pkgs.parsec pkgs.parsec3-numbers pkgs.parallel pkgs.ordered-containers pkgs.vector pkgs.containers pkgs.stm-linkedlist ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.ghcid ];
}
