{
  description = "delator structured tracing for OCaml";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2;
    in {
      packages.${system}.default = ocamlPackages.buildDunePackage {
        pname = "delator";
        version = "0.1.0";
        src = self;
        duneVersion = "3";
        propagatedBuildInputs = [ ocamlPackages.ppxlib ];
        doCheck = true;
      };

      devShells.${system}.default = pkgs.mkShell {
        packages = [ ocamlPackages.ocaml ocamlPackages.dune_3 ocamlPackages.findlib ocamlPackages.ppxlib ];
      };
    };
}
