{
  description = "Tools for working with todo-cli code";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs =
    { self, nixpkgs }:

    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskell.compiler.ghc912
          haskellPackages.cabal-fmt
          haskellPackages.hoogle
          pkg-config
          libpq.pg_config
          postgresql
          ormolu
        ];
      };
    };
}
