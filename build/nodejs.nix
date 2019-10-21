{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghcjs"
}:
with nixpkgs;
let
  haskellPackages' = import ./haskell.nix { inherit nixpkgs compiler; };

  ease = package: with haskell.lib;
    ( doJailbreak
    ( dontHaddock
    ( dontCheck
    ( package
    ))));

  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          aeson                = ease hspkgs.aeson;
          bifunctors           = ease hspkgs.bifunctors;
          exceptions           = ease hspkgs.exceptions;
          hashable             = ease hspkgs.hashable;
          hspec                = ease hspkgs.hspec;
          hspec-discover       = ease hspkgs.hspec-discover;
          scientific           = ease hspkgs.scientific;
          lens                 = ease hspkgs.lens;
          comonad              = ease hspkgs.comonad;
          semigroupoids        = ease hspkgs.semigroupoids;
          invariant            = ease hspkgs.invariant;
          optparse-applicative = ease hspkgs.optparse-applicative;
          uuid-types           = ease hspkgs.uuid-types;
          adjunctions          = ease hspkgs.adjunctions;
          QuickCheck           = ease hspkgs.QuickCheck;
        });
  });

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    platform-types
    platform-dsl
    platform-process
  ]);

in
stdenv.mkDerivation rec {
  name = "platform-env";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
