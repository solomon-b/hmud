{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers, hpack
      , lens, megaparsec, mtl, network, parsers, persistent
      , raw-strings-qq, sqlite-simple, stdenv, stm, text
      , text-conversions, transformers, trifecta, unliftio-core
      }:
      mkDerivation {
        pname = "hmud";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base bytestring containers lens megaparsec mtl network
          parsers persistent raw-strings-qq sqlite-simple stm text
          text-conversions transformers trifecta unliftio-core
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          async base bytestring containers lens megaparsec mtl network
          parsers persistent raw-strings-qq sqlite-simple stm text
          text-conversions transformers trifecta unliftio-core
        ];
        prePatch = "hpack";
        homepage = "https://github.com/ssbothwell/hMud#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
