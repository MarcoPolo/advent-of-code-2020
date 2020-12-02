{ pkgs }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.random ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.hello ];
}
