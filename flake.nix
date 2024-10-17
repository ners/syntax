{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    semi-iso = {
      url = "github:ners/semi-iso";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) ./.;
      };
      readDirs = root: attrNames (lib.filterAttrs (_: type: type == "directory") (readDir root));
      readFiles = root: attrNames (lib.filterAttrs (_: type: type == "regular") (readDir root));
      basename = path: suffix: with lib; pipe path [
        (splitString "/")
        last
        (removeSuffix suffix)
      ];
      cabalProjectPackages = root: with lib; foreach (readDirs root) (dir:
        let
          path = "${root}/${dir}";
          files = readFiles path;
          cabalFiles = filter (strings.hasSuffix ".cabal") files;
          pnames = map (path: basename path ".cabal") cabalFiles;
          pname = if pnames == [ ] then null else head pnames;
        in
        optionalAttrs (pname != null) { ${pname} = path; }
      );
      cabalProjectPnames = root: lib.attrNames (cabalProjectPackages root);
      cabalProjectOverlay = root: hfinal: hprev: with lib;
        mapAttrs
          (pname: path: hfinal.callCabal2nix pname path { })
          (cabalProjectPackages root);
      project = hsSrc ./.;
      pnames = cabalProjectPnames project;
      hpsFor = pkgs: with lib;
        { default = pkgs.haskellPackages; }
        // filterAttrs
          (name: hp: match "ghc[0-9]{2}" name != null && versionAtLeast hp.ghc.version "9.2")
          pkgs.haskell.packages;
      overlay = lib.composeManyExtensions [
        inputs.semi-iso.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay project)
            ];
          };
        })
      ];
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          name = "syntax";
          bin-pnames = filter (pname: lib.strings.hasPrefix "syntax-example" pname) pnames;
          lib-pnames = filter (pname: ! lib.strings.hasPrefix "syntax-example" pname) pnames;
          bins = pkgs.buildEnv {
            name = "${name}-bins";
            paths = map (pname: hps.default.${pname}) bin-pnames;
            pathsToLink = [ "/bin" ];
          };
          libs = pkgs.buildEnv {
            name = "${name}-libs";
            paths = lib.mapCartesianProduct
              ({ hp, pname }: hp.${pname})
              { hp = attrValues hps; pname = lib-pnames; };
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.buildEnv {
            name = "${name}-docs";
            paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) lib-pnames;
          };
          sdist = pkgs.buildEnv {
            name = "${name}-sdist";
            paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) lib-pnames;
          };
          docsAndSdist = pkgs.linkFarm "${name}-docsAndSdist" { inherit docs sdist; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${name}-all";
            paths = [ bins libs docsAndSdist ];
            inherit (hps.default.syntax) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with hp; [
                  pkgs'.haskellPackages.cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      );
}
