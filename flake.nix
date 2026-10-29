{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/26.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.agda.url = "github:agda/agda/59f6359"; # Older versions have JS backend bugs
  outputs = inputs:
    (inputs.flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          agda = inputs.agda.outputs.packages.${system}.default.bin;
          gallery = pkgs.stdenv.mkDerivation  # Builds the examples gallery
            {
              name = "iepje-examples";
              src = ./.;
              buildInputs = [ agda ];
              LC_ALL="C.utf8";
              buildPhase = "bash compile.sh";
              installPhase = "mv web/ $out";
            };
          svgs = pkgs.stdenv.mkDerivation  # Pretty-prints the source to SVG
            {
              name = "svgs";
              src = ./.;
              buildInputs = [ agda pkgs.wkhtmltopdf pkgs.fontconfig ];
              FONTCONFIG_FILE = pkgs.makeFontsConf
                {
                  fontDirectories =
                    [
                      (pkgs.iosevka-bin.override { variant = "SGr-IosevkaFixedSS12"; })
                      (pkgs.iosevka-bin.override { variant = "SGr-IosevkaFixed"; })
                    ];
                };
              configurePhase = "export HOME=$(mktemp -d)";
              buildPhase = "bash export-code-to-svg.sh";
              installPhase = "mv _build/svg/ $out";
            };
          devShell = pkgs.mkShell # For developing Iepje
            {
              packages =
                [
                  pkgs.darkhttpd
                  pkgs.inotify-tools
                  pkgs.xdg-utils
                ];
              inputsFrom = [ gallery svgs ];
            };
        in
        {
          packages.default  = gallery;
          packages.gallery  = gallery;
          packages.svgs     = svgs;
          devShells.default = devShell;
        }
      )
    );
}
