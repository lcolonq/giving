{
  description = "shell for giving";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      buildInputs = [
        pkgs.rgbds
      ];
      shell = pkgs.mkShell {
        inherit buildInputs;
      };
    in {
      defaultPackage.x86_64-linux = shell;
      devShell.x86_64-linux = shell;
      packages.x86_64-linux = {
        inherit shell;
      };
    };
}
