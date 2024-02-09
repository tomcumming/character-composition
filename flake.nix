{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixpkgs-fmt;

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.iconv

          pkgs.rustc
          pkgs.cargo
          pkgs.rustfmt
          pkgs.rust-analyzer
        ];
      };
    };
}
