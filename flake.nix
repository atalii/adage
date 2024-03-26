{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-23.11";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in pkgs.mkShell {
        buildInputs = with pkgs; [
          alire gprbuild gnat libxcrypt
          spark2014 z3 cvc4
        ];
      };
  };
}
