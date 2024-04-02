{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let 
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; overlays = [ myNixPkgsOverlay ]; };

      myNixPkgsOverlay = (nixSelf: nixSuper: {
	myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
	  overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
	});
      });
      myHaskellPkgsOverlay = (hSelf: hSuper: {
	myProject = hSelf.callCabal2nix "fun-gadt" ./. {};
      });

      myDevTools = with pkgs; [
	ghc
	cabal-install
      ];
    in
    {
      devShells.${system}.default = 
	pkgs.mkShell {
	  packages = myDevTools; 
	};
    };
}
