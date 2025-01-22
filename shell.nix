let
  nixpkgs = import sources.nixpkgs {};
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
  pre-commit-check = import ./nix/pre-commit.nix;
  sources = import ./nix/sources.nix;
in
  with nixpkgs;
  with nixpkgs.haskellPackages;
    shellFor rec {
      packages = p: [(import ./.)];
      buildInputs = [
        cabal-install
        haskell-language-server
        hlint
        fourmolu
        ghcid
        procps

        clang-tools

        gdb
        valgrind
      ];
      shellHook = ''
        ${pre-commit-check.shellHook}
      '';

      LD_LIBRARY_PATH = nixpkgs.lib.makeLibraryPath buildInputs;
    }
