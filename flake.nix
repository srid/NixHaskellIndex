{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.project = {
          devShell.tools = hp:
            let
              # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
              fixCyclicReference = drv:
                pkgs.haskell.lib.overrideCabal drv (_: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt
                foreman;
              inherit (pkgs.haskellPackages)
                cabal-fmt tailwind;
              inherit (hp)
                fourmolu;
              ghcid = fixCyclicReference hp.ghcid;
              haskell-language-server = hp.haskell-language-server.overrideScope (lself: lsuper: {
                ormolu = fixCyclicReference hp.ormolu;
              });
            };
          settings = {
            NixHaskellIndex = { self, super, ... }: {
              custom = _: super.NixHaskellIndex.overrideAttrs (_: {
                DATAFILE = config.packages.data;
              });
            };
          };
        };
        apps.default = self'.apps.project-NixHaskellIndex;
        packages = {
          default = self'.packages.project-NixHaskellIndex;
          data = pkgs.writeTextFile {
            name = "data";
            text =
              let data = import ./src/NHI/data.nix { inherit inputs pkgs lib; };
              in builtins.toJSON data;
          };
          site = pkgs.runCommand "site"
            { }
            ''
              mkdir -p $out
              pushd ${self}
              ${lib.getExe config.packages.default} \
                gen $out
              ${lib.getExe pkgs.haskellPackages.tailwind} \
                -o $out/tailwind.css 'src/**/*.hs' 
            '';
        };
        devShells.default = config.devShells.project.overrideAttrs (_: {
          DATAFILE = config.packages.data;
        });
      };
    };
}
