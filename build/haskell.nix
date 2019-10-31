{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
rec {
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

  terraformSrc = fetchGit {
    url = https://github.com/gwils/terraform-hs;
    ref = "support-more-ghcs";
    rev = "9815ba708b5b936e003cb14f83885ee89f7a5e6c";
  };

  platformTypesSrc      = ../platform-types;
  platformDSLSrc        = ../platform-dsl;
  platformAWSSrc        = ../platform-aws;
  platformKubeSrc       = ../platform-kube;
  platformTerraformSrc  = ../platform-terraform;
  platformProcessSrc    = ../platform-process;
  platformVisualSrc     = ../platform-visual;

  projectPackages = hspkgs: {
    executor           = ease hspkgs.executor;
    terraform-hs       = hspkgs.callCabal2nix "terraform-hs"       "${terraformSrc}" {};
    stratosphere       = hspkgs.callCabal2nix "stratosphere"       "${stratosphereSrc}" {};
    platform-types     = hspkgs.callCabal2nix "platform-types"     "${platformTypesSrc}" {};
    platform-dsl       = hspkgs.callCabal2nix "platform-dsl"       "${platformDSLSrc}" {};
    platform-aws       = hspkgs.callCabal2nix "platform-aws"       "${platformAWSSrc}" {};
    platform-terraform = hspkgs.callCabal2nix "platform-terraform" "${platformTerraformSrc}" {};
    platform-kube      = hspkgs.callCabal2nix "platform-kube"      "${platformKubeSrc}" {};
    platform-process   = hspkgs.callCabal2nix "platform-process"   "${platformProcessSrc}" {};
    platform-visual    = hspkgs.callCabal2nix "platform-visual"    "${platformVisualSrc}" {};
  };

  packages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs:
        projectPackages hspkgs
      );
  });
}
