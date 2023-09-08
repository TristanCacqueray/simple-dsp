{
  nixConfig.bash-prompt = "[nix(simple-dsp)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/fe0dabfd8acf96f1b5cff55766de6284517868cf";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        simple-dsp = hpPrev.callCabal2nix "simple-dsp" self { };
        # Bump requested at https://github.com/ekmett/gl/issues/24
        gl = pkgs.haskell.lib.doJailbreak hpPrev.gl;
        dear-imgui = pkgs.haskell.lib.overrideCabal
          (pkgs.haskell.lib.doJailbreak hpPrev.dear-imgui) (drv: {
            src = pkgs.fetchFromGitHub {
              owner = "haskell-game";
              repo = "dear-imgui.hs";
              rev = "eec8b57ce89236456ce9d79c760e6abc5d0c27a3";
              sha256 = "sha256-JRBfTCBkMJEQBMGG8kZq/nGIaMyioUnQkchVOjiJX3U=";
              fetchSubmodules = true;
            };
          });
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      baseTools = with pkgs; [
        hpack
        cabal-install
        hsPkgs.cabal-fmt
        hlint
        fourmolu
        weeder
        hsPkgs.doctest
        ffmpeg
      ];

    in {
      packages."x86_64-linux".default =
        pkgs.haskell.lib.justStaticExecutables hsPkgs.hdsp-demo;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.simple-dsp ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
