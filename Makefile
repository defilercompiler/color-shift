build:
	hlint Main.hs
	stylish-haskell -i Main.hs
	stack build $(STACK_OPTS) --copy-bins
dev:
	ghcid --command "stack ghci color-shift"

.PHONY: nix-build
nix-build:
	nix-build -A exe
