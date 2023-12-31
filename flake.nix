{
  inputs = {
    "nixos-22.11".url = "github:NixOS/nixpkgs/nixos-22.11";
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    "nixos-23.11".url = "github:NixOS/nixpkgs/nixos-23.11";
    "nixos-unstable".url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "ascii-group";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-22.11" = import inputs."nixos-22.11" { inherit system; };
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
          "nixos-23.11" = import inputs."nixos-23.11" { inherit system; };
          "nixos-unstable" = import inputs."nixos-unstable" { inherit system; };
        };
        pkgs = nixpkgs."nixos-22.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./ascii-group;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

        ascii-char =
          { mkDerivation, base, hashable, hspec, lib }:
          mkDerivation {
            pname = "ascii-char";
            version = "1.0.1.0";
            sha256 = "9b56ef31b90e0ef697e7624c8054e957cf155d3df68a71318766e837b81f9aba";
            revision = "2";
            editedCabalFile = "1x0ci7j3bdlrrza78n53xw4y1dl4py3gqrym0lb6l9w5n7l138gs";
            libraryHaskellDepends = [ base hashable ];
            testHaskellDepends = [ base hspec ];
            homepage = "https://github.com/typeclasses/ascii-char";
            description = "A Char type representing an ASCII character";
            license = lib.licenses.asl20;
          };

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;

            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            in (pkgs.haskell.packages.${ghcVersion}.override (old: {
              inherit (nixpkgs.nixos-unstable) all-cabal-hashes;
              overrides = combineOverrides old [
                (packageSourceOverrides { ascii-group = ./ascii-group; })
                overrides
              ];

            })).ascii-group;

          in rec {
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc92";
              overrides = new: old: {
                hashable = new.callHackage "hashable" "1.4.0.2" { };
                hspec = new.callHackage "hspec" "2.10.0.1" { };
                hspec-core = dontCheck (new.callHackage "hspec-core" "2.10.0.1" { });
                hspec-discover = new.callHackage "hspec-discover" "2.10.0.1" { };
                hspec-meta = new.callHackage "hspec-meta" "2.9.3" { };
              };
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc94";
              overrides = new: old: {
                hashable = new.callHackage "hashable" "1.4.1.0" { };
              };
            };
            ghc-9-6 = makeTestConfiguration {
              ghcVersion = "ghc96";
              pkgs = nixpkgs."nixos-23.11";
              overrides = new: old: {
                ascii-char = new.callPackage ascii-char { };
              };
            };
            ghc-9-8 = makeTestConfiguration {
              ghcVersion = "ghc98";
              pkgs = nixpkgs."nixos-23.11";
              overrides = new: old: {
                ascii-char = new.callPackage ascii-char { };
                hashable = new.callHackage "hashable" "1.4.3.0" { };
                hspec = new.callHackage "hspec" "2.11.7" { };
                hspec-core = dontCheck (new.callHackage "hspec-core" "2.11.7" { });
                hspec-discover = new.callHackage "hspec-discover" "2.11.7" { };
                hspec-expectations = new.callHackage "hspec-expectations" "0.8.4" { };
                hspec-meta = new.callHackage "hspec-meta" "2.11.7" { };
                tagged = new.callHackage "tagged" "0.8.8" { };
              };
            };
            all = pkgs.symlinkJoin {
              name = packageName;
              paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
            };
          };
        };
      });
}
