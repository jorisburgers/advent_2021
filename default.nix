let
    pkgs = import <nixpkgs> {};
    ghc = pkgs.haskell.packages.ghc901.ghcWithPackages(ps: []);

in with pkgs; buildEnv 
    {   name = "advent-env";
        paths = [
              # Haskell
              ghc
              stack
        ];
    }