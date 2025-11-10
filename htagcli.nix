pkgs:
pkgs.haskell.lib.overrideCabal
  (pkgs.haskellPackages.callCabal2nix "htagcli" ./. { })
  (old:
  let
    isStatic = pkgs.stdenv.hostPlatform.isMusl;
    libffiStatic = pkgs.libffi.overrideAttrs
      (old: { dontDisableStatic = true; });
    gmp6Static = pkgs.gmp6.override { withStatic = true; };
    taglibStatic = pkgs.taglib.overrideAttrs (old:
      {
        dontDisableStatic = true;
        cmakeFlags = [ (pkgs.lib.cmakeBool "BUILD_SHARED_LIBS" false) ];
      });
    # elfutils static lib in 25.05, doesn't include eu* symbols
    elfutilsStatic = (pkgs.elfutils.overrideAttrs (old: rec{
      dontDisableStatic = true;
      version = "0.193";

      src = pkgs.fetchurl {
        url = "https://sourceware.org/elfutils/ftp/${version}/${old.pname}-${version}.tar.bz2";
        hash = "sha256-eFf0S2JPTY1CHfhRqq57FALP5rzdLYBJ8V/AfT3edjU=";
      };
    }));
    zstdStatic = pkgs.zstd.override { enableStatic = true; };
    bzip2Static = pkgs.bzip2.override { enableStatic = true; };
    xzStatic = pkgs.xz.override { enableStatic = true; };
  in
  {
    enableLibraryProfiling = false;
    enableSharedExecutables = !isStatic;
    enableSharedLibraries = !isStatic;
    # doesn't work when linking statically
    doCheck = !isStatic;
    configureFlags = (old.configureFlags or [ ]) ++ pkgs.lib.optionals isStatic [
      "--enable-executable-static"
      "--disable-executable-dynamic"
      "--ghc-option=-optl=-static"
      "--ghc-option=-static"

      "--extra-lib-dirs=${libffiStatic}/lib"
      "--extra-lib-dirs=${gmp6Static}/lib"

      # taglib and dependencies
      "--extra-lib-dirs=${taglibStatic}/lib"
      "--ghc-option=-optl=-ltag"
      "--ghc-option=-optl=-lstdc++"

      "--extra-lib-dirs=${elfutilsStatic.out}/lib"
      "--ghc-option=-optl-lelf"

      # elfutils dependencies
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--ghc-option=-optl-lz"
      "--extra-lib-dirs=${zstdStatic.out}/lib"
      "--ghc-option=-optl-lzstd"
      "--extra-lib-dirs=${bzip2Static.out}/lib"
      "--ghc-option=-optl-lbz2"
      "--extra-lib-dirs=${xzStatic.out}/lib"
      "--ghc-option=-optl-llzma"
    ];
  })
