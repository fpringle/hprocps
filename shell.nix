args@{ packages ? ""
, ...
}:
let
  nixpkgs = import ./nix/nixpkgs.nix args;
  pre-commit-check = import ./nix/pre-commit.nix;
  monorepo = import ./hprocps.nix args;
  allPackages = builtins.attrNames monorepo;
  shell-packages =
    if packages == ""
    then allPackages
    else nixpkgs.lib.strings.splitString "," packages;
in
with nixpkgs;
with nixpkgs.haskellPackages;
shellFor rec {
  packages = p: nixpkgs.lib.attrVals shell-packages p;
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
