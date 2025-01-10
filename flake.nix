{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/24.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.agda.url = "github:agda/agda/master";
  outputs = { self, nixpkgs, flake-utils, agda }:
    (flake-utils.lib.eachDefaultSystem (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Entry point for `nix build` and `nix develop`
        packages.default = pkgs.stdenv.mkDerivation {
          name = "iepje-examples";
          src = ./.;
          buildInputs = [
            pkgs.entr
            pkgs.findutils
            agda.outputs.packages.${system}.default
            ];
          buildPhase = ''
            bash compile.sh;
            '';
          installPhase = ''
            mkdir $out
            cp -r _build $out/_build/
            cp -r src $out/src/
          '';
        };
      }
    ));
}
