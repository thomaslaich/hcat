{
  description = "srid/hcat: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            # Packages to add on top of `basePackages`
            packages = {
              # Add source or Hackage overrides here
              # (Local packages are added automatically)
              /*
                aeson.source = "1.5.0.0" # Hackage version
                shower.source = inputs.shower; # Flake input
              */
            };

            # Add your package overrides here
            settings = {
              /*
                hcat = {
                  haddock = false;
                };
                aeson = {
                  check = false;
                };
              */
            };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";

            programs.ormolu.enable = true;
            programs.nixfmt.enable = true;
            programs.cabal-fmt.enable = true;
            programs.hlint.enable = true;
            programs.yamlfmt.enable = true;

            # We use fourmolu
            programs.ormolu.package = pkgs.haskellPackages.fourmolu;
            settings.formatter.ormolu = {
              options = [
                "--ghc-opt"
                "-XImportQualifiedPost"
              ];
            };
          };

          # Default package & app.
          packages.default = self'.packages.hcat;
          apps.default = self'.apps.hcat;

          # Default shell.
          devShells.default = pkgs.mkShell {
            name = "hcat";
            meta.description = "Haskell development environment";
            # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];
            nativeBuildInputs = with pkgs; [ just ];
          };
        };
    };
}
