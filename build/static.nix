{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
let
  staticHaskellNixSrc = fetchGit {
    url = https://github.com/nh2/static-haskell-nix;
    rev = "ff7715e0e13fb3f615e64a8d8c2e43faa4429b0f";
  };

  cabal2nixOverlay = self: super:
    { buildPackages = super.buildPackages // {
        cabal2nix = super.haskellPackages.cabal2nix;
      };
    };

  pkgs = (import (staticHaskellNixSrc + "/nixpkgs.nix")).pkgsMusl.appendOverlays [cabal2nixOverlay];

  surveyPath = staticHaskellNixSrc + "/survey/default.nix";
  survey = ((import surveyPath) { normalPkgs = pkgs;
                                  approach = "pkgsMusl";
                                  integer-simple = true;
                                }
           );


  haskell = import ./haskell.nix { inherit compiler;
                                           nixpkgs = pkgs;
                                           haskellPackages = survey.haskellPackages;
                                 };
  ease = haskell.ease;
  haskellPackages' = haskell.packages;


  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs: {
        tls = ease hspkgs.tls;
      });
  });



  buildStatic = drv: with pkgs.haskell.lib;
    ( disableSharedExecutables
    ( disableSharedLibraries
    ( nixpkgs.lib.flip appendConfigureFlags
        [ "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ]
    ( drv
    ))));

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    (buildStatic platform-types)
    (buildStatic platform-dsl)
    (buildStatic platform-aws)
    (buildStatic platform-kube)
    (buildStatic platform-process)
    (buildStatic platform-visual)
  ]);

in
pkgs.stdenv.mkDerivation rec {
  name = "platform-static-env";

  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
