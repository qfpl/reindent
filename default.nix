{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  hpython = pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "hpython";
    rev = "49bd11644bbda2a7d3732180212ecad08e2a7760"; # develop
    sha256 = "0b7kzw42nkb50n3nlczll60imk7nhg746jh0ch4nn3b26xbwmwdp";
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      digit          = self.callHackage "digit" "0.7" {};
      hpython        = import hpython {};
      validation = pkgs.haskell.lib.dontCheck (self.callHackage "validation" "1" {});
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reindent.nix {};

in
  drv
  # if pkgs.lib.inNixShell then drv.env else drv
