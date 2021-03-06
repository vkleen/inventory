{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "master";
    };

    flake-utils.url = "github:numtide/flake-utils";
    haskell-language-server = {
      type = "github";
      owner = "haskell";
      repo = "haskell-language-server";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hledger-src = {
      url = "github:vkleen/hledger";
      flake = false;
    };
  };

  description = "Inventory Management for crazy people";

  outputs = { self, ... }@inputs:
    let
      ghcVersion = "901";

      inherit (inputs.nixpkgs) lib;
      overlays = system: [
        (final: prev: {
          sources = prev.sources or {} // {
            hledger-src = inputs.hledger-src;
          };
        })
      ];

      pkgs = lib.mapAttrs (system: pkgs:
          import pkgs.path { inherit system; config = { allowBroken = true; }; overlays = overlays system;}
        ) inputs.nixpkgs.legacyPackages;
      forAllSystems = f: lib.mapAttrs f pkgs;

      shell = s: p: let
        haskellTool = t: p.haskell.lib.justStaticExecutables p.haskell.packages."ghc${ghcVersion}".${t};
      in p.mkShell {
        packages = [
          p.haskell.packages."ghc${ghcVersion}".ghc
          inputs.haskell-language-server.packages.${s}."haskell-language-server-${ghcVersion}"
          (haskellTool "cabal-install")
          (haskellTool "ghcid")
          (haskellTool "hlint")
          (haskellTool "hpack")

          p.bazel p.python3
        ];
      };
    in {
      nixpkgs = forAllSystems (_: p: p);
      devShell = forAllSystems shell;
  };
}
