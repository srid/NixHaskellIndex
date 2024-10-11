# Nix to generate data to be parsed into NHI/Types.hs
{ inputs, pkgss, lib }:
let
  groupLibraries = packageSet: builtins.groupBy (x: x.pname)
    (lib.mapAttrsToList
      (n: v: {
        name = n;
        inherit (v) pname version;
        inherit (v.meta) broken;
      })
      (lib.filterAttrs (n: v: builtins.typeOf v == "set" && lib.hasAttr "pname" v)
        packageSet));
in
{
  nixpkgsRev = inputs.nixpkgs.rev;
  packages =
    let
      #ghcPkgSets =
      #  lib.attrValues (lib.filterAttrs (_: set: lib.hasAttr "ghc" set) pkgs.haskell.packages)
      #  ++ [ pkgs.haskellPackages ];
      ghcPkgSets = {
        "" = pkgss.horizon.platform.master;
        "horizon-core (master)" = pkgss.horizon.core.master;
        "horizon-advance (master)" = pkgss.horizon.advance.master;
        "horizon-platform (master)" = pkgss.horizon.platform.master;
        "horizon-devtools (master)" = pkgss.horizon.devtools.master;
        "horizon-plutus (master)" = pkgss.horizon.plutus.master;
        "horizon-cardano (master)" = pkgss.horizon.cardano.master;
        "nixpkgs (24.05)" = pkgss.nixpkgs."2405".haskell.packages.ghc964;
        "nixpkgs (24.11)" = pkgss.nixpkgs."2411".haskell.packages.ghc964;
      };
    in
    lib.mapAttrs
      (_: set: groupLibraries set)
      ghcPkgSets;
}
