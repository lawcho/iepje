{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/24.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.agda.url = "github:agda/agda/59f6359"; # Older versions have JS backend bugs
  outputs = { self, nixpkgs, flake-utils, agda }:
    (flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkg = pkgs.stdenv.mkDerivation  # For building the examples gallery
            {
              name = "iepje-examples";
              src = ./.;
              buildInputs = [ agda.outputs.packages.${system}.default.bin ];
              buildPhase = ''
                bash compile.sh;
                '';
              installPhase = ''
                mkdir $out
                cp -r web/. $out
              '';
            };
          devShell = pkgs.mkShell # For developing Iepje
            {
              packages = [pkgs.darkhttpd pkgs.inotify-tools pkgs.xdg-utils];
              inputsFrom = [ pkg ];
            };
        in
        {
          packages.default = pkg;
          devShells.default = devShell;
        }
      )
    );
}
