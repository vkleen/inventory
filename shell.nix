let devShell = (builtins.getFlake (builtins.toString ./.)).ghcShell."${builtins.currentSystem}";
in devShell
# pkgs.mkShell {
#   buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (p: [ p.hledger-lib p.cryptonite ])) ];
# }
