{ compiler ? "ghc865"
, nixpkgs ? import (import ./nix/sources.nix).nixpkgs {}
}:

let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  hsPkgs = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "color-shift" =
        self.callCabal2nix
          "color-shift"
          (gitignore ./.)
          {};
    };
  };

  shell = hsPkgs.shellFor {
    packages = ps: [
      ps."color-shift"
    ];

    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hlint
      hpack
      hsPkgs.cabal-install
      stack
      stylish-haskell
    ];

    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (hsPkgs."color-shift");

in { inherit shell; inherit exe; }
