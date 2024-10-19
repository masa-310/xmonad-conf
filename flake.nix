{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        #"x86_64-darwin"
        #"aarch64-linux"
        #"aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            xmonad-config-project =
              final.haskell-nix.project {
                src = ./.;
                  #evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          #hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "925" ]; };
        flake = pkgs.xmonad-config-project.flake {};
      in flake // {
        defaultPackage = flake.packages."xmonad-config:exe:xmonad-config";
        packages.xmonad-config = flake.packages."xmonad-config:exe:xmonad-config";
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            stack
                #hls
          ];
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
