{
  description = "Opt-in Stack Flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs =
          pkgs.haskell.packages."ghc927"; # need to match Stackage LTS version from stack.yaml resolver

        myDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
        ];

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
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;

          shellHook = ''
            if test -f "REPO_UNINITIALIZED.sh"; then
              chmod +x REPO_UNINITIALIZED.sh
              ./REPO_UNINITIALIZED.sh
              rm REPO_UNINITIALIZED.sh
            fi
          '';
        };
      });
}
