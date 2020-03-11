{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
let
  happySrc = fetchGit {
    url = https://github.com/simonmar/happy;
    rev = "3ea1aa76d9a2e87f23fde830b1e42d62e20c3f4c";
  };

  cabal2nixOverlay = self: super:
    { buildPackages = super.buildPackages // {
        cabal2nix = super.haskellPackages.cabal2nix;
      };
    };

  staticHaskellNixpkgsSrc = fetchTarball https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
  pkgs = (import staticHaskellNixpkgsSrc { config.allowBroken = true; }).pkgsMusl.appendOverlays [cabal2nixOverlay];

  staticHaskellNixSrc = fetchGit {
    url = https://github.com/nh2/static-haskell-nix;
    rev = "761f34bb4b09dd5838f82782c5e56ebfac4039fb";
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
        tls             = ease hspkgs.tls;
        http-client-tls = ease hspkgs.http-client-tls;
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
    (buildStatic platform-harness)
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
