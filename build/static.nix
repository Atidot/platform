{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
let
  cabal2nixOverlay = self: super:
    { buildPackages = super.buildPackages // {
        cabal2nix = super.haskellPackages.cabal2nix;
      };
    };

  staticHaskellNixpkgsSrc = fetchTarball https://github.com/nh2/nixpkgs/archive/d9cc01374235745ea8581174c4335ae9dda86504.tar.gz;
  pkgs = (import staticHaskellNixpkgsSrc { config.allowBroken = true; }).pkgsMusl.appendOverlays [cabal2nixOverlay];

  staticHaskellNixSrc = fetchGit {
    url = https://github.com/nh2/static-haskell-nix;
    rev = "b66fee31b10663588cc23dc806a75f393749d30d";
  };

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
        tls      = ease hspkgs.tls;
        graphviz = ease hspkgs.graphviz;
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
