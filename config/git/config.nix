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
        gpg = {
          textconv = "gpg --no-tty --decrypt";
        };
        sopsdiffer = {
          textconv = "sops -d";
        };
      };

      difftool = {
        dyff.cmd = "dyff --color auto between --omit-header \"$LOCAL\" \"$REMOTE\"";
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

    includes =
      let
        inherit (config.accounts.email) accounts;
        personal = {
          signing.key =
            lib.optional
              (lib.hasAttrByPath [ "gpg" "key" ] accounts.personal)
              accounts.personal.gpg.key;
          user.email = accounts.personal.address;
        };
        work = {
          signing.key =
            lib.optional
              (lib.hasAttrByPath [ "gpg" "key" ] accounts.work)
              accounts.work.gpg.key;
          user.email = accounts.work.address;
        };
      in
      [
        {
          condition = "gitdir:~/src/git.sr.ht/";
          contents = personal;
        }
        {
          condition = "gitdir:~/src/github.com/";
          contents = personal;
        }
        {
          condition = "gitdir:~/src/gitlab.com/";
          contents = personal;
        }
        {
          condition = "gitdir:~/src/gitlab.sportradar.ag/";
          contents = work;
        }
      ];

    lfs.enable = true;

    userName =
      (builtins.head
        (lib.filter (account: account.primary)
          (lib.attrValues config.accounts.email.accounts))).realName;
  };

  xdg.configFile."git/attributes".text = ''
    *.gpg filter=gpg diff=gpg
  '';

  xdg.configFile."pass-git-helper/git-pass-mapping.ini" = {
    source = ./git-pass-mapping.ini;
  };

  xdg.dataFile."git/commit.template" = {
    source = ./commit.template;
  };

}
