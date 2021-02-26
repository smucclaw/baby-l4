{
  description = "A flake for installing the dependencies of baby l4";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  # inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;
  inputs.gf-nix.url = github:anka-213/cclaw-nix-stuff/nix-flakes;
  inputs.gf-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, gf-nix }: {

    devShell = builtins.mapAttrs
      (arch: pkgs:
        let
          myStack = pkgs.haskell.lib.appendPatches
            pkgs.haskellPackages.stack
            [
              (pkgs.fetchpatch {
                url = "https://github.com/commercialhaskell/stack/pull/5490.patch";
                sha256 = "15782z8ggkzmxs8bl9lpp598vbydhb8rr6gv42ajaadaj6q6kcxv";
              })
            ];
          gf-pkgs = gf-nix.packages.${arch};
          # inherit (gf-pkgs) gf-wordnet;
        in
        pkgs.mkShell {
          nativeBuildInputs = [
            myStack
            gf-pkgs.gf-with-rgl
            # pkgs.graphviz
            # pkgs.emacs
          ];
          # export NIX_PATH=nixpkgs=${nixpkgs};
          # export GF_LIB_PATH=$GF_LIB_PATH''${GF_LIB_PATH:+':'}${gf-pkgs.gf-wordnet}
          shellHook = ''
            export GF_LIB_PATH=${gf-pkgs.gf-rgl}/rgl:${gf-pkgs.gf-wordnet}
          '';
        })
      nixpkgs.legacyPackages;
  };
}
