{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
let
  ease = package: with haskell.lib;
    ( doJailbreak
    ( dontHaddock
    ( dontCheck
    ( package
    ))));

  stratosphereSrc = fetchGit {
    url = https://github.com/freckle/stratosphere;
    rev = "64e7bfb3abcad278e6160cd411abdd21a485a671";
  };

  platformTypesSrc   = ../platform-types;
  platformDSLSrc     = ../platform-dsl;
  platformAWSSrc     = ../platform-aws;
  platformKubeSrc    = ../platform-kube;
  platformProcessSrc = ../platform-process;
  platformVisualSrc  = ../platform-visual;

  projectPackages = hspkgs: {
    stratosphere     = hspkgs.callCabal2nix "stratosphere"     "${stratosphereSrc}" {};
    platform-types   = hspkgs.callCabal2nix "platform-types"   "${platformTypesSrc}" {};
    platform-dsl     = hspkgs.callCabal2nix "platform-dsl"     "${platformDSLSrc}" {};
    platform-aws     = hspkgs.callCabal2nix "platform-aws"     "${platformAWSSrc}" {};
    platform-kube    = hspkgs.callCabal2nix "platform-kube"    "${platformKubeSrc}" {};
    platform-process = hspkgs.callCabal2nix "platform-process" "${platformProcessSrc}" {};
    platform-visual  = hspkgs.callCabal2nix "platform-visual"  "${platformVisualSrc}" {};
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides
    (self: hspkgs:
      projectPackages hspkgs
    );
})
