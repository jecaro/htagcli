{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  # description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";

  inputs.htaglib-src =
    {
      url = "github:jecaro/htaglib/add-property-api";
      flake = false;
    };

  outputs = { self, nixpkgs, htaglib-src }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        htagcli = final.haskellPackages.callCabal2nix "htagcli" ./. { };

        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            htaglib = prev.haskell.lib.addExtraLibrary
              (hprev.callCabal2nix "htaglib" htaglib-src { })
              final.taglib;
          };
        };

      });
      packages = forAllSystems (system: {
        htagcli = nixpkgsFor.${system}.htagcli;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.htagcli);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.htagcli ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            cabal-gild
            cabal-install
            ghcid
            haskell-language-server
          ];
        });
    };
}
