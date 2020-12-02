{ pkgs }:
let ghc = pkgs.haskellPackages.ghcWithHoogle (pkgs: [ pkgs.split ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.hello ];
}
