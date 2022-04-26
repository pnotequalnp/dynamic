{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            final.lib.composeExtensions prev.haskell.packageOverrides
            (hsFinal: hsPrev: {
              dynamic = hsFinal.callCabal2nix "dynamic" ./. { };
            });
        };
      };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        inherit (pkgs.lib) composeExtensions optional;
        inherit (pkgs.haskell.packages) ghc8107 ghc921;
        tools = with ghc921; [ cabal-install fourmolu hlint pkgs.nixfmt ];
        devShell = lsp: hs:
          hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ dynamic ];
            nativeBuildInputs = tools
              ++ optional lsp [ hs.haskell-language-server ];
          };
      in {
        packages = {
          ghc921 = ghc921.dynamic;
          ghc8107 = ghc8107.dynamic;
          default = ghc921.dynamic;
        };

        devShells = {
          default = devShell true ghc921;
          ghc921 = devShell true ghc921;
          ghc8107 = devShell true ghc8107;
          ci = devShell false ghc921;
        };
      }) // {
        overlays.default = overlay;
      };
}
