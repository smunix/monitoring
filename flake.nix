{
  description = "A simulation of monitoring systems";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
  };
  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay =
            final:
            _:
            with final;
            with haskell.lib;
            with haskellPackages;
            {
              health =
                overrideCabal
                  (callCabal2nix "health" ./. {})
                  (o:
                    {
                      version = o.version + "-${version}";
                    });
            };
          overlays = [ overlay ];
          pkgs = import nixpkgs { inherit system overlays; };
      in
        {
          packages = {
            inherit (pkgs) health;
          };
        });
}
