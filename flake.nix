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
      ghcVersion = "8104";

      inherit (inputs.nixpkgs) lib;
      overlays = system: [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = hfinal: hprev:
            {
              hledger = hfinal.callCabal2nixWithOptions "hledger" inputs.hledger-src "--subpath=hledger" {};
              hledger-lib = hfinal.callCabal2nixWithOptions "hledger-lib" inputs.hledger-src "--subpath=hledger-lib" {};
              hledger-ui = hfinal.callCabal2nixWithOptions "hledger-ui" inputs.hledger-src "--subpath=hledger-ui" {};
              hledger-web = hfinal.callCabal2nixWithOptions "hledger-web" inputs.hledger-src "--subpath=hledger-web" {};
            };
          };
        })
      ];


      pkgs = lib.mapAttrs (system: pkgs:
          import pkgs.path { inherit system; config = { allowBroken = true; }; overlays = overlays system;}
        ) inputs.nixpkgs.legacyPackages;
      forAllSystems = f: lib.mapAttrs f pkgs;

      pkg = _: p: {
        inv-mgmt = p.haskell.packages."ghc${ghcVersion}".callCabal2nix "inv-mgmt" "${self}/mgmt" {};
      };

      shell = s: p: p.mkShell {
        packages = [
          inputs.haskell-language-server.packages.${s}."haskell-language-server-${ghcVersion}"
          p.haskell.packages."ghc${ghcVersion}".cabal-install
          p.haskell.packages."ghc${ghcVersion}".ghcid
          p.hpack
        ];
        inputsFrom = [ self.legacyPackages."${s}".inv-mgmt.env ];
      };

      ghcShell = s: p: p.mkShell {
        packages = [
          (p.haskell.packages."ghc${ghcVersion}".ghcWithPackages (g: [ self.legacyPackages."${s}".inv-mgmt ]))
        ];
      };
    in {
      legacyPackages = forAllSystems pkg;
      devShell = forAllSystems shell;
      ghcShell = forAllSystems ghcShell;

      defaultPackage = forAllSystems (s: _: self.legacyPackages."${s}".inv-mgmt);
  };
}
