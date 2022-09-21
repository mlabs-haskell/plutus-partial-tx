.PHONY: hoogle build-all format lint requires_nix_shell services \
		stop-services build-lucid-lib build-frontend watch-frontend

build-all:
	nix -L --show-trace build .#packages.x86_64-linux

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

services: requires_nix_shell
	@cd testnet; rm -f $$PWD/node/node.socket; \
	./start-node.sh > node.log & \
	echo "Waiting for node.socket, please wait...."; \
	until [ -S $$PWD/node/node.socket ]; do sleep 1; done; \
	./start-cix.sh > cix.log &

stop-services:
	pkill -SIGINT cardano-node; pkill -SIGINT plutus-chain-in

serve: requires_nix_shell
	@CARDANO_NODE_SOCKET_PATH=$$PWD/testnet/node/node.socket cabal run partial-tx-server

build-lucid-lib: requires_nix_shell
	@cd lucid-partialtx && deno run --allow-env --allow-write --allow-read --allow-net --allow-run build.ts; \

build-frontend: requires_nix_shell
	@cd example/frontend && npx webpack

watch-frontend: requires_nix_shell
	@cd example/frontend && npx webpack --watch

query-tip: requires_nix_shell
	@CARDANO_NODE_SOCKET_PATH=$$PWD/testnet/node/node.socket cardano-cli query tip --testnet-magic 2

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

EXTENSIONS := -o -XTypeApplications -o -XPatternSynonyms

# Run fourmolu and deno formatter
format: requires_nix_shell
	env -C . fourmolu -i --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs)
	deno fmt lucid-partialtx
	nixpkgs-fmt $(NIX_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

# Check formatting (without making changes)
format_check:
	env -C . fourmolu --mode check --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs)
	deno fmt lucid-partialtx --check
	nixpkgs-fmt --check $(NIX_SOURCES)
	cabal-fmt -c $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell fd -enix)
CABAL_SOURCES := $(shell fd -ecabal)

nixfmt: requires_nix_shell
	nixfmt $(NIX_SOURCES)

nixfmt_check: requires_nix_shell
	nixfmt --check $(NIX_SOURCES)

# Apply hlint suggestions
lint: requires_nix_shell
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check: requires_nix_shell
	hlint $(shell fd -ehs)

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)
