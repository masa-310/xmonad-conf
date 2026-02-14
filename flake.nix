{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # a dependency of Stack, that does not have this bug: https://github.com/commercialhaskell/rio/issues/264
  # inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=pull/466258/head";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        #"x86_64-darwin"
        #"aarch64-linux"
        #"aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            xmonad-config-project = final.haskell-nix.project {
              src = ./.;
              #evalSystem = "x86_64-linux";
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        # should be matched with stack ghc version
        hPkgs = pkgs.haskell.packages."ghc9103";
        # Wrap Stack to work with our Nix integration. We do not want to modify
        # stack.yaml so non-Nix users do not notice anything.
        # - no-nix: We do not want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
        #hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "925" ]; };
        flake = pkgs.xmonad-config-project.flake { };
        devTools = [
          hPkgs.ghc
          hPkgs.haskell-language-server
          hPkgs.implicit-hie
          stack-wrapped
        ];
      in
      flake
      // {
        defaultPackage = flake.packages."xmonad-config:exe:xmonad-config";
        packages.xmonad-config = flake.packages."xmonad-config:exe:xmonad-config";
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            pkg-config
            autoconf
            gcc
            zlib
            xorg.libX11
            xorg.libXext
            xorg.libXft
            xorg.libXinerama
            xorg.libXpm
            xorg.libXrandr
            xorg.libXrender
            xorg.libXScrnSaver
            libxcb
            libxdmcp
            expat
          ];
          buildInputs = devTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
      }
    );

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
