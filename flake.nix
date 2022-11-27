{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell overrides
    ema.url = "github:srid/ema";
    ema.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.project = {
          packages.NixHaskellIndex.root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt
              foreman;
            inherit (pkgs.haskellPackages)
              cabal-fmt tailwind;
            inherit (hp)
              fourmolu;
          };
          source-overrides = {
            ema = inputs.ema + /ema;
            ema-generics = inputs.ema + /ema-generics;
            ema-extra = inputs.ema + /ema-extra;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            ema-generics = dontCheck super.ema-generics;
            NixHaskellIndex = super.NixHaskellIndex.overrideAttrs (_: {
              DATAFILE = config.packages.data;
            });
          };
        };
        apps.tailwind.program = pkgs.haskellPackages.tailwind;
        packages.data = pkgs.writeTextFile {
          name = "data";
          text =
            let data = import ./src/NHI/data.nix { inherit inputs pkgs lib; };
            in builtins.toJSON data;
        };
        packages.default = config.packages.project-NixHaskellIndex;
        devShells.default = config.devShells.project.overrideAttrs (_: {
          DATAFILE = config.packages.data;
        });
      };
    };
}
