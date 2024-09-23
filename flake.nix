{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  # description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        hs-tag = final.haskellPackages.callCabal2nix "hs-tag" ./. { };
      });
      packages = forAllSystems (system: {
        hs-tag = nixpkgsFor.${system}.hs-tag;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.hs-tag);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.hs-tag ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        });
    };
}
