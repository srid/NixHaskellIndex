# Nix to generate data to be parsed into NHI/Types.hs
{ pkgs, lib }:
builtins.groupBy (x: x.pname)
  (lib.mapAttrsToList
    (n: v: {
      name = n;
      inherit (v) pname version;
      inherit (v.meta) broken;
    })
    (lib.filterAttrs (n: v: builtins.typeOf v == "set" && lib.hasAttr "pname" v)
      pkgs.haskellPackages))
