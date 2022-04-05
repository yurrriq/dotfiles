{ config, lib, pkgs, ... }:

{

  programs.git = {
    delta = {
      enable = true;
      options = {
        plus-style = "syntax #012800";
        minus-style = "syntax #340001";
        syntax-theme = "Monokai Extended";
        navigate = true;
      };
    };

    extraConfig = {
      color = {
        diff-highlight = {
          oldNormal = "red bold";
          oldHighlight = "red bold 52";
          newNormal = "green bold";
          newHighlight = "green bold 22";
        };

        diff = {
          meta = 227;
          frag = "magenta bold";
          commit = "227 bold";
          old = "red bold";
          new = "green bold";
          whitespace = "red reverse";
        };

        status = {
          added = "green";
          changed = "yellow";
          untracked = "cyan";
        };
        ui = true;
      };

      commit.template = "${config.xdg.dataHome}/git/commit.template";

      credential = {
        helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
        useHttpPath = true;
      };

      diff = {
        sopsdiffer = {
          textconv = "sops -d";
        };
      };

      difftool = {
        pdfdiffer.cmd = "diff-pdf --view \"$LOCAL\" \"$REMOTE\"";
        prompt = false;
        trustExitCode = true;
      };

      fetch.prune = true;

      init.defaultBranch = "main";

      pull.ff = "only";

      rerere.enabled = true;

      url."git@gitlab.sportradar.ag:" = {
        insteadOf = "https://gitlab.sportradar.ag/";
      };
    };

    lfs.enable = true;
  };

  xdg.configFile."pass-git-helper/git-pass-mapping.ini" = {
    source = ./git-pass-mapping.ini;
  };

  xdg.dataFile."git/commit.template" = {
    source = ./commit.template;
  };

}
