{ indexState, pkgs, ... }:

let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.rocksdb
    ];
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "rocksdb-haskell";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.rocksdb-haskell =
    project.hsPkgs.rocksdb-haskell-jprupp.components.library;
  packages.spec =
    project.hsPkgs.rocksdb-haskell-jprupp.components.tests.spec;
}
