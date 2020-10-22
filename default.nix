{ pkgs ? (import ./pkgs.nix {}) } :
let
  hashids = haskellPackages :
    pkgs.haskell.lib.dontCheck (
      haskellPackages.callCabal2nix "hashids-st" (pkgs.fetchFromGitHub {
        owner  = "foxhound-systems";
        repo   = "hashids-st";
        rev    = "2e5bb3c88d34d1d8456cc9e5a923e49d6b234e56";
        sha256 = "0r3cyy3i4xr8ra1l2fnd13nvrxmrfl5qzy4zs532q66apy9fvdxz";
      }) {});
  mkPackage = pkgs :
    pkgs.haskellPackages.developPackage
      { root = ./.;
        name = "obfuscate";
        overrides = self: super: {
            hashids-st = hashids super;
        };
        source-overrides = {
        };
      };
in
mkPackage pkgs
