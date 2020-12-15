{ pkgs ? import <nixpkgs> { } }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.split pkgs.parsec pkgs.parsec3-numbers pkgs.parallel ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.hello ];
}
