{
  description = "Plutus Partial Tx";

  inputs = {
    nixpkgs.follows = "bot-plutus-interface/nixpkgs";
    haskell-nix.follows = "bot-plutus-interface/haskell-nix";

    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface?ref=gergely/vasil";

    cardano-node.url = "github:input-output-hk/cardano-node?ref=7612a245a6e2c51d0f1c3e0d65d7fe9363850043";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, bot-plutus-interface, cardano-node, ... }:
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

      deferPluginErrors = true;

      projectFor = system:
        let
          # For adding cardano exes to the nix shell.
          cardano-exes = [
            cardano-node.apps.${system}.cardano-node
          ] ++ (with cardano-node.apps.${system}; [
            cardano-cli
            cardano-submit-api
          ]);
          cardanoExesPath = builtins.concatStringsSep ":" (map (x: builtins.dirOf x.program) cardano-exes);
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
              nativeBuildInputs = (with pkgs'; [
                cabal-install
                fd
                haskellPackages.apply-refact
                haskellPackages.cabal-fmt
                hlint
                nixpkgs-fmt
                deno
                nodejs
                nodePackages.npm
                haskellPackages.fourmolu
              ]) ++ (with project.hsPkgs; [
                plutus-chain-index.components.exes.plutus-chain-index
              ]);

              tools.haskell-language-server = { };

              additional = ps: [
                ps.bot-plutus-interface
              ];

              # Add the cardano exes to PATH.
              shellHook = ''
                export PATH=$PATH:${cardanoExesPath}
              '';
            };
          };
        in
        project;
    in
    {
      inherit nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
