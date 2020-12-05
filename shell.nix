{ pkgs }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.split pkgs.parsec pkgs.parsec3-numbers ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.hello ];
}
