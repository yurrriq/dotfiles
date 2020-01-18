{ ... }:

{

  programs.git = {

    enable = true;

    aliases = rec {
      bm = "branch --merged";
      bnm = "branch --no-merged";
      co = "checkout";
      cob = "${co} -b";
      ca = "commit --amend";
      cam = "${ca} -m";
      can = "${ca} --no-edit";
      cans = "${can} -S";
      cas = "${ca} -S";
      casm = "${cas} -m";
      cm = "commit -m";
      csm = "commit -S -m";
      cs = "crypt status";
      cse = "crypt status -e";
      csu = "crypt status -u";
      d = "diff";
      dc = "diff --cached";
      ds = "diff --stat";
      ffco = "flow feature checkout";
      ffr = "flow feature rebase";
      ffs = "flow feature start";
      frs = "flow release start";
      frf = "flow release finish";
      frfs = "flow release finish -s";
      tree = "log --all --graph --oneline";
      rb = "rebase";
      rba = "rebase --abort";
      rbc = "rebase --continue";
      rbi = "rebase --interactive";
      rbs = "rebase --skip";
      r = "reset";
      rh = "reset --hard";
      sl = "stash list";
      sa = "stash apply";
      sk = "stash --keep-index";
      sp = "stash pop";
      spa = "stash --patch";
      ss = "stash save";
      st = "status -s";
      stat = "status";
      rest = "reset";
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
      };
      # TODO: commit.template = "~/.gitmessage";
      core.pager = "diff-so-fancy | less --tabs=4 -RFX";
      credential = {
        helper = "pass-git-helper";
        useHttpPath = true;
      };
      diff = {
        tool = "kitty";

        sopsdiffer = {
          textconv = "sops -d";
        };
      };
      difftool = {
        prompt = false;
        trustExitCode = true;

        kitty.cmd = "kitty +kitten diff $LOCAL $REMOTE";
        pdfdiffer.cmd = "diff-pdf --view \"$LOCAL\" \"$REMOTE\"";
      };

      rerere.enabled = true;
      ui.color = true;
    };

    ignores = [
      "*~"
      ".DS_Store"
    ];

    lfs.enable = true;

    signing.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";

    userEmail = "e.bailey@sportradar.com";

    userName = "Eric Bailey";

  };

}
