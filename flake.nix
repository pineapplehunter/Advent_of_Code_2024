{
  description = "A basic shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    {
      nixpkgs,
      systems,
      ...
    }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      pkgsFor = system: import nixpkgs { inherit system; };
    in
    {
      devShells = eachSystem (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            packages = [
              (pkgs.sbcl.withPackages (sp: [
                sp.str
                sp.cl-ppcre
              ]))
              (pkgs.python3.withPackages (ps: [ ps.ipython ]))
            ];
          };
        }
      );

      legacyPackages = eachSystem pkgsFor;
    };
}
