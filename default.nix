{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  hpython = pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "hpython";
    rev = "f1371dad6951343f804852f88c4cb8c78baae565"; # develop
    sha256 = "1bzam5rccmgg74zwkd1ccmkx2cs4nvmh09qxw5xggf5dxz4a8wgl";
  };

  validation = pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    rev = "b7c5c0cf7d0bdbd8c0c47ac03d3a83bc75fc5f57";
    sha256 = "1hkd7ma38rdq9rw0bbivi3l33i01idy5qka2x9fmzvfkjzr17q1q";
  };

  modifiedHaskellPackages =
    haskellPackages.override {
      overrides = self: super: {
        hpython = pkgs.haskell.lib.dontCheck (self.callCabal2nix "hpython" hpython {});
	hedgehog = self.callHackage "hedgehog" "0.6" {};
        validation = self.callCabal2nix "validation" validation {};
      };
    };

  drv = modifiedHaskellPackages.callPackage ./reindent.nix {};

in
  drv
  # if pkgs.lib.inNixShell then drv.env else drv
