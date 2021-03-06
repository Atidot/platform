{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghcjs"
}:
with nixpkgs;
let
  haskell = import ./haskell.nix { inherit nixpkgs compiler; };
  ease    = haskell.ease;
  haskellPackages' = haskell.packages;

  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          comonad          = ease hspkgs.comonad;
          http-types       = ease hspkgs.http-types;
          lens             = ease hspkgs.lens;
          aeson            = ease hspkgs.aeson;
          semigroupoids    = ease hspkgs.semigroupoids;
          exceptions       = ease hspkgs.exceptions;
          bifunctors       = ease hspkgs.bifunctors;
          QuickCheck       = ease hspkgs.QuickCheck;
          tasty-quickcheck = ease hspkgs.tasty-quickcheck;
          scientific       = ease hspkgs.scientific;
          temporary        = ease hspkgs.temporary;
          graphviz         = ease hspkgs.graphviz;
        });
  });

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    platform-types
    platform-dsl
    #platform-aws
    #platform-kube
    #platform-process
    platform-visual
  ]);

in
stdenv.mkDerivation rec {
  name = "platform-js-env";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
