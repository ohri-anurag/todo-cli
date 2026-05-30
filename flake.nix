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
      dockerImage = pkgs.dockerTools.buildLayeredImage {
        name = "ghcr.io/ohri-anurag/ci-generator";
        tag = "latest";
        contents = with pkgs; [
          haskellPackages.cabal-install
          haskell.compiler.ghc912
          bash # Needed by docker to run commands inside an image
          coreutils # mkdir
          wget # Needed by cabal to fetch updates
          pkg-config # Needed by rel8
          libpq.pg_config # Needed by rel8
          gnused # Needed by postgresql-libpq-configure
          gnugrep # Needed by postgresql-libpq-configure
          gawk # Needed by postgresql-libpq-configure
        ];
      };
    };
}
