{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = [ 
    pkgs.esbuild
    pkgs.nodejs_20
  ];
}
