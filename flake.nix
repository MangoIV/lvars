{
  description = "lvish";
  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.haskell-flake.url = "github:srid/haskell-flake";

  inputs.concurrent-skiplist.url = "github:rrnewton/concurrent-skiplist";
  inputs.concurrent-skiplist.flake = false;
  inputs.pbbs-haskell.url = "github:adk9/pbbs-haskell";
  inputs.pbbs-haskell.flake = false;

  outputs = inputs: inputs.parts.lib.mkFlake  { inherit inputs; }{
    systems = [ "x86_64-linux" ];

    imports = [
      inputs.haskell-flake.flakeModule 
      inputs.pre-commit-hooks.flakeModule
    ];

    perSystem = { config, pkgs, ... }: {
      haskellProjects.default = {
        basePackages = pkgs.haskell.packages.ghc92;
        packages = {
          concurrent-skiplist.source = inputs.concurrent-skiplist;
          pbbs-haskell.source = inputs.pbbs-haskell;
        };
        settings = { 
          chaselev-deque = {
            jailbreak = true;
            broken = false;
          };
          bits-atomic = {
            jailbreak = true;
            broken = false;
          };
          hsbencher = {
            jailbreak = true;
            broken = false;
          };
        };
      };

    };
  };
}
