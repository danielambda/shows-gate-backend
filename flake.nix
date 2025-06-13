{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = { self', config, pkgs, ... }:
        let
          shows-gate-backend = self'.packages.shows-gate-backend;
          shows-gate-backend-image = pkgs.dockerTools.buildLayeredImage {
            name = "shows-gate-backend";
            tag = "latest";
            contents = [shows-gate-backend pkgs.cacert];
            config = {
              Expose = [8080];
              Entrypoint = ["${shows-gate-backend}/bin/web-api"];
            };
          };
        in {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd];

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';
        };

        packages = {
          default = shows-gate-backend;
          inherit shows-gate-backend-image;
        };
      };
    };
}
