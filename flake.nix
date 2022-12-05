{
  description = "A flake for installing the dependencies of baby l4";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.nixpkgs-newer.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  # inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;
  inputs.gf-nix.url = github:anka-213/cclaw-nix-stuff/nix-flakes;
  inputs.gf-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, nixpkgs-newer, gf-nix }: {

    devShell = builtins.mapAttrs
      (arch: pkgs:
        let
          gf-pkgs = gf-nix.packages.${arch};
          # inherit (gf-pkgs) gf-wordnet;
        in
        pkgs.mkShell {
          nativeBuildInputs = [
            gf-pkgs.gf-with-rgl
            pkgs.haskell.compiler.ghc924
            nixpkgs-newer.legacyPackages.${arch}.stack
            # pkgs.graphviz
            # pkgs.emacs
          ];
          shellHook = ''
            export NIX_PATH=nixpkgs=${nixpkgs};
            export GF_LIB_PATH=$GF_LIB_PATH''${GF_LIB_PATH:+':'}${gf-pkgs.gf-rgl}/rgl:${gf-pkgs.gf-wordnet}
          '';
        })
      nixpkgs.legacyPackages;
  };
}
