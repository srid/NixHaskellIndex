# Nix to generate data to be parsed into NHI/Types.hs
{ inputs, pkgs, lib, system }:
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
        "" = pkgs.haskellPackages;
        "9.4.3" = pkgs.haskell.packages.ghc943;
        "9.2.5" = pkgs.haskell.packages.ghc925;
        "horizon-9.4.2" = inputs.horizon-platform.packages.${system};
        "horizon-plutus-9.4.2" = inputs.horizon-plutus.packages.${system};
      };
    in
    lib.mapAttrs
      (_: set: groupLibraries set)
      ghcPkgSets;
}
