NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz

build:
	nix-build ${NIX_FLAGS}

build-js:
	nix-build ${NIX_FLAGS} nodejs.nix

build-static:
	nix-build ${NIX_FLAGS} static.nix --show-trace

shell:
	nix-shell ${NIX_FLAGS}

notebook:
	nix-shell ${NIX_FLAGS} -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"

docker:
	nix-build ${NIX_FLAGS} docker.nix -o docker.tar.gz
