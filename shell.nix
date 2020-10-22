let
  defaultPkgs =
    import ./pkgs.nix { };

  project =
    import ./default.nix { };

  extraBuildInputs = pkgs : [
      pkgs.gnumake
      pkgs.ghcid
      pkgs.cabal-install
    ];
in
project.overrideAttrs (attrs:
  {
    buildInputs = attrs.buildInputs ++ (extraBuildInputs defaultPkgs);
  })
