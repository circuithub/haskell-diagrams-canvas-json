{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    just-flake.url = "github:juspay/just-flake";
    haskell-gerber.url = "github:circuithub/haskell-gerber/f3f6ce800a5f0bf4ec214d3f9e56069bbf5e3055";
    haskell-gerber.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.just-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          packages = {
            gerber.source = inputs.haskell-gerber + /gerber;
            gerber-diagrams.source = inputs.haskell-gerber + /gerber-diagrams;
          };

          devShell = {
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
            hlsCheck.enable = false;
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          flakeFormatter = false; # For https://github.com/numtide/treefmt-nix/issues/55

          programs.prettier.enable = true;
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = false;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Dev shell scripts.
        just-flake.features = {
          treefmt.enable = true;
        };

        # Default package & app.
        packages.default = self'.packages.diagrams-canvas-json-dev;

        # Default shell.
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.just-flake.outputs.devShell
          ];

          buildInputs = [
            pkgs.nodejs
            pkgs.nodePackages_latest.typescript-language-server
            pkgs.nodePackages_latest.typescript
          ];
        };
      };
    };
}
