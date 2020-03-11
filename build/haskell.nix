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

  makeStatic = package: with haskell.lib;
    ( disableSharedExecutables
    ( disableSharedLibraries
    ( nixpkgs.lib.flip appendConfigureFlags
        [ "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ]
    ( package
    ))));

  stratosphereSrc = fetchGit {
    url = https://github.com/freckle/stratosphere;
    rev = "64e7bfb3abcad278e6160cd411abdd21a485a671";
  };

  terraformHsSrc = fetchGit {
    url = https://github.com/atidot/terraform-hs;
    rev = "4815301f4d5cf8343907ea55e41f4f491930dc69";
  };

  platformTypesSrc          = ../platform-types;
  platformDSLSrc            = ../platform-dsl;
  platformAWSSrc            = ../platform-aws;
  platformHarnessSrc        = ../platform-harness;
  platformKubeSrc           = ../platform-kube;
  platformProcessSrc        = ../platform-process;
  platformPackagingSrc      = ../platform-packaging;
  platformPackagingTypesSrc = ../platform-packaging-types;
  platformVisualSrc         = ../platform-visual;
  platformDeploymentSrc     = ../platform-deployment;

  projectPackages = hspkgs: {
    language-python          = ease hspkgs.language-python;
    platform-harness         = makeStatic (hspkgs.callCabal2nix "platform-harness" "${platformHarnessSrc}" {});
    stratosphere             = hspkgs.callCabal2nix "stratosphere"             "${stratosphereSrc}" {};
    terraform-hs             = hspkgs.callCabal2nix "terraform-hs"             "${terraformHsSrc}"  {};
    platform-types           = hspkgs.callCabal2nix "platform-types"           "${platformTypesSrc}" {};
    platform-dsl             = hspkgs.callCabal2nix "platform-dsl"             "${platformDSLSrc}" {};
    platform-aws             = hspkgs.callCabal2nix "platform-aws"             "${platformAWSSrc}" {};
    platform-kube            = hspkgs.callCabal2nix "platform-kube"            "${platformKubeSrc}" {};
    platform-packaging       = hspkgs.callCabal2nix "platform-packaging"       "${platformPackagingSrc}" {};
    platform-packaging-types = hspkgs.callCabal2nix "platform-packaging-types" "${platformPackagingTypesSrc}" {};
    platform-process         = hspkgs.callCabal2nix "platform-process"         "${platformProcessSrc}" {};
    platform-visual          = hspkgs.callCabal2nix "platform-visual"          "${platformVisualSrc}" {};
    platform-deployment      = hspkgs.callCabal2nix "platform-deployment"      "${platformDeploymentSrc}" {};
  };

  packages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs:
        projectPackages hspkgs
      );
  });
}
