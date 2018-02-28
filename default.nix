{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, brick, bytestring, config-ini
      , directory, either, microlens, postgresql-simple, process, SHA
      , stdenv, text, time, transformers, vector, vty
      }:
      mkDerivation {
        pname = "cyanide";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        executableHaskellDepends = [
          base brick bytestring config-ini directory either microlens
          postgresql-simple process SHA text time transformers vector vty
        ];
        homepage = "https://github.com/dgonyeo/cyanide#readme";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
