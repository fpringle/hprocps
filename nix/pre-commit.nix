let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs {};
  nix-pre-commit-hooks =
    import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");

  cFilePattern = "\\.(c|h)$";
  haskellFilePattern = "\\.l?hs(-boot)?$";
  nixFilePattern = "\\.nix$";

  hlint = "${nixpkgs.haskellPackages.hlint}/bin/hlint";
  fourmolu = "${nixpkgs.haskellPackages.fourmolu}/bin/fourmolu";
  clang-format = "${nixpkgs.clang-tools}/bin/clang-format";
  nixpkgs-fmt = "${nixpkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
in
  nix-pre-commit-hooks.run {
    src = ./.;

    hooks = {
      "0-hlint" = {
        name = "hlint";
        enable = true;
        description =
          "HLint gives suggestions on how to improve your source code.";
        entry = "bash -c 'for n in $(seq 0 \"$#\"); do ${hlint} --refactor --refactor-options=\"-i\" \"\${!n}\"; done'";
        files = haskellFilePattern;
      };

      "1-fourmolu" = {
        name = "fourmolu";
        enable = true;
        description = "Haskell code prettifier.";
        entry = "${fourmolu} --mode inplace";
        files = haskellFilePattern;
      };

      "2-clang-format" = {
        name = "clang-format";
        enable = true;
        description = "Format C files prettifier.";
        entry = "${clang-format}";
        files = cFilePattern;
      };

      "3-nixpkgs-format" = {
        name = "nixpkgs-fmt";
        enable = true;
        description = "Nix code formatter for nixpkgs.";
        entry = "${nixpkgs-fmt}";
        files = nixFilePattern;
      };

      # by default, pre-commit fails if a hook modifies files, but doesn't
      # tell us which files have been modified. Smart, right?
      # this workaround runs a `git diff` to print any files that have
      # been modified by previous hooks.
      # NOTE: this should always be the last hook run, so when adding hooks
      # make sure to add them above this one.
      "4-git-diff" = {
        name = "git diff";
        enable = true;
        entry = "git diff --name-only --exit-code";
        language = "system";
        pass_filenames = true;
      };
    };
  }
