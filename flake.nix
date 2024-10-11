{
  description = "Ema template app";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    horizon-core.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-core";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-devtools";
    horizon-plutus.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-plutus";
    horizon-cardano.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-cardano";

    nixpkgs-2405.url = "github:nixos/nixpkgs/24.05";
    nixpkgs-2411.url = "github:nixos/nixpkgs/24.11-pre";

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, system, ... }:
        let pkgss = {
              horizon = {
                core = {
                  master = inputs.horizon-core.packages.x86_64-linux;
                };
                advance = {
                  master = inputs.horizon-advance.packages.x86_64-linux;
                };
                platform = {
                  master = inputs.horizon-platform.packages.x86_64-linux;
                };
                devtools = {
                  master = inputs.horizon-devtools.packages.x86_64-linux;
                };
                plutus = {
                  master = inputs.horizon-platform.packages.x86_64-linux;
                };
                cardano = {
                  master = inputs.horizon-cardano.packages.x86_64-linux;
                };
              };
              nixpkgs = {
                "2405" = import inputs.nixpkgs-2405 { inherit system; };
                "2411" = import inputs.nixpkgs-2411 { inherit system; };
              };
            };
        in
        {
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
              let data = import ./src/NHI/data.nix { inherit inputs pkgss lib; };
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
