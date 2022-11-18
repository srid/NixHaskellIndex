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
  haskellPackages = groupLibraries pkgs.haskellPackages;
  packages =
    let
      #ghcPkgSets =
      #  lib.attrValues (lib.filterAttrs (_: set: lib.hasAttr "ghc" set) pkgs.haskell.packages)
      #  ++ [ pkgs.haskellPackages ];
      ghcPkgSets = {
        "" = pkgs.haskellPackages;
        "9.4.3" = pkgs.haskell.packages.ghc943;
        "9.2.5" = pkgs.haskell.packages.ghc925;
      };
    in
    lib.mapAttrs
      (_: set: groupLibraries set)
      ghcPkgSets;
}
