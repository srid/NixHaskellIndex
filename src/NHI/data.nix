# Nix to generate data to be parsed into NHI/Types.hs
{ inputs, pkgs, lib }:
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
        "9.4.5" = pkgs.haskell.packages.ghc945;
        "9.6.1" = pkgs.haskell.packages.ghc961;
      };
    in
    lib.mapAttrs
      (_: set: groupLibraries set)
      ghcPkgSets;
}
