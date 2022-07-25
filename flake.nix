{
  description = "Plutus Partial Tx";

  inputs = {
    nixpkgs.follows = "bot-plutus-interface/nixpkgs";
    haskell-nix.follows = "bot-plutus-interface/haskell-nix";

    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, bot-plutus-interface, ... }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${bot-plutus-interface.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              pkgs'.haskellPackages.fourmolu
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      deferPluginErrors = true;

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc8107";
            inherit (bot-plutus-interface) cabalProjectLocal;
            extraSources = bot-plutus-interface.extraSources ++ [
              {
                src = bot-plutus-interface;
                subdirs = [ "." ];
              }
            ];
            modules = bot-plutus-interface.haskellModules;

            shell = {
              withHoogle = true;

              exactDeps = true;

              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.fd
                pkgs'.haskellPackages.apply-refact
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.hlint
                pkgs'.nixpkgs-fmt
                pkgs'.haskellPackages.fourmolu
              ];

              tools.haskell-language-server = { };

              additional = ps: [
                ps.bot-plutus-interface
              ];
            };
          };
        in
        project;
    in
    {
      inherit nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      checks = perSystem (system:
        self.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
        }
      );
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [ self.devShell.inputDerivation ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      packages = perSystem (system: self.flake.${system}.packages);
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
