# [Advent of Code 2020](https://adventofcode.com/) in Haskell

Run programs with `runhaskell day-n.hs`.

# Setting up the environment

Enter a development shell with `nix develop`.
Integrates with `direnv` + flakes with
`https://github.com/nix-community/nix-direnv`

# One liner with flake
```
nix develop github:marcopolo/advent-of-code-2020  --command runhaskell day-1.hs
```