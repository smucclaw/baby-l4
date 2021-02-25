{ pkgs ? import <nixpkgs> { } }:
let
  # inherit (import ./dep/gitignoresrc { inherit (pkgs) lib; }) gitignoreSource;

  myStack = pkgs.haskell.lib.appendPatches
    pkgs.haskellPackages.stack
    [
      (pkgs.fetchpatch {
        url = "https://github.com/commercialhaskell/stack/pull/5490.patch";
        sha256 = "15782z8ggkzmxs8bl9lpp598vbydhb8rr6gv42ajaadaj6q6kcxv";
      })
    ];

in
pkgs.mkShell {
  nativeBuildInputs =
    [
      # cabal-install
      # cabal-fmt
      # ghcid
      # haskell-language-server
      myStack
    ];
}
