{ pkgs }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.random pkgs.list ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.hello ];
}
