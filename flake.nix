{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/24.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.agda.url = "github:agda/agda/59f6359"; # Older versions have JS backend bugs
  outputs = { self, nixpkgs, flake-utils, agda }:
    (flake-utils.lib.eachDefaultSystem (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Entry point for `nix build` and `nix develop`
        packages.default = pkgs.stdenv.mkDerivation {
          name = "iepje-examples";
          src = ./.;
          buildInputs = [
            pkgs.darkhttpd
            pkgs.inotify-tools
            pkgs.xdg-utils
            agda.outputs.packages.${system}.default.bin
            ];
          buildPhase = ''
            bash compile.sh;
            '';
          installPhase = ''
            mkdir $out
            cp -r web/. $out
          '';
        };
      }
    ));
}
