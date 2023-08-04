{
  description = "lvish";
  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.haskell-flake.url = "github:srid/haskell-flake";

  inputs.concurrent-skiplist.url = "github:rrnewton/concurrent-skiplist";
  inputs.concurrent-skiplist.flake = false;
  inputs.pbbs-haskell.url = "github:adk9/pbbs-haskell";
  inputs.pbbs-haskell.flake = false;
  inputs.haskell-lockfree.url = "github:rrnewton/haskell-lockfree";
  inputs.haskell-lockfree.flake = false;
  inputs.ctrie.url = "github:iu-parfunc/ctrie";
  inputs.ctrie.flake = false;

  outputs = inputs:
    inputs.parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = {
        config,
        pkgs,
        ...
      }: {
        pre-commit = {
          check.enable = true;
          settings = {
            hooks = {
              alejandra.enable = true;
              deadnix.enable = true;
              statix.enable = true;

              # hlint obviously fails
              hlint.enable = false;
              # stylish-haskell cannot parse the project
              stylish-haskell.enable = false;
            };
          };
        };

        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages = {
            concurrent-skiplist.source = inputs.concurrent-skiplist;
            pbbs-haskell.source = inputs.pbbs-haskell;
            chaselev-deque.source = "${inputs.haskell-lockfree}/chaselev-deque";
            hlint.source = "3.6.1";
            ctrie.source = inputs.ctrie;
          };
          devShell.mkShellArgs.shellHook = config.pre-commit.installationScript;
          settings = {
            bits-atomic = {
              jailbreak = true;
              broken = false;
            };
            hsbencher = {
              jailbreak = true;
              broken = false;
            };
            concurrent-skiplist.jailbreak = true;
            lattices.jailbreak = true;
            chaselev-deque.jailbreak = true;
          };
        };
      };
    };
}
