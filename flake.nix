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
    miso = {
      url = "github:haskell-miso/miso";
      flake = false;
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
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) root;
      };
      projects =
        with lib;
        genAttrs'
          (fileset.toList (fileset.fileFilter (file: file.hasExt "cabal") ./.))
          (file: nameValuePair (removeSuffix ".cabal" (baseNameOf file)) (dirOf file));
      pnames = attrNames projects;
      haskell-overlay = lib.composeManyExtensions [
        inputs.semi-iso.overlays.haskell
        (hfinal: _: lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (sourceFilter ./${pname}) { }))
        (hfinal: _: {
          miso = hfinal.callCabal2nix "miso" inputs.miso { };
        })
      ];
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            haskell-overlay
          ];
        };
      };
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          name = "syntax";
          hps = with lib; foldlAttrs
            (acc: name: hp':
              let
                hp = tryEval hp';
                version = getVersion hp.value.ghc;
                majorMinor = versions.majorMinor version;
                ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
              in
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
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
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-install
                  fourmolu
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
