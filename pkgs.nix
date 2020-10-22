let
  pkgsVersion = builtins.fetchGit (
      builtins.fromJSON (builtins.readFile ./nixpkgs.json)
    );
in
pkgArgs :
  import pkgsVersion pkgArgs
