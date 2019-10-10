{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
let
  isGHCJS = lib.hasPrefix "ghcjs" compiler;
  ease = package: haskell.lib.doJailbreak (haskell.lib.dontHaddock (haskell.lib.dontCheck package));

  #----
  fixesGHCJS = hspkgs: if isGHCJS then {
    lens             = ease hspkgs.lens;
    comonad          = ease hspkgs.comonad;
    semigroupoids    = ease hspkgs.semigroupoids;
    QuickCheck       = ease hspkgs.QuickCheck;
    tasty-quickcheck = ease hspkgs.tasty-quickcheck;
    scientific       = ease hspkgs.scientific;
    temporary        = ease hspkgs.temporary;
  } else {};

  #----
  platformTypesSrc   = ../platform-types;
  platformDSLSrc     = ../platform-dsl;
  platformAWSSrc     = ../platform-aws;
  platformKubeSrc    = ../platform-kube;
  platformProcessSrc = ../platform-process;
  platformVisualSrc  = ../platform-visual;

  projectPackages = hspkgs: {
    platform-types   = hspkgs.callCabal2nix "platform-types"  "${platformTypesSrc}" {};
    platform-dsl     = hspkgs.callCabal2nix "platform-sdl"    "${platformDSLSrc}" {};
    platform-aws     = hspkgs.callCabal2nix "platform-aws"    "${platformAWSSrc}" {};
    platform-kube    = hspkgs.callCabal2nix "platform-kube"   "${platformKubeSrc}" {};
    platform-process = hspkgs.callCabal2nix "platform-proess" "${platformProcessSrc}" {};
    platform-visual  = hspkgs.callCabal2nix "platform-visual" "${platformVisualSrc}" {};
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides
    (self: hspkgs:
      fixesGHCJS hspkgs
   // projectPackages hspkgs
    );
})
