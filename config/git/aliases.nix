{ config, lib, ... }:

{

  programs.fish.shellAbbrs = lib.mkIf (config.programs.fish.enable) {
    g = "git";
    ga = "gita";
    gaf = "gita fetch";
    gall = "gita ll";
    gd = "git diff";
    gdc = "git diff --cached";
    gm = "git merge";
    gs = "git status --short --untracked-files=no";
    gt = "git tree";
  };

  programs.git.aliases = rec {
    ap = "add --patch";
    bm = "branch --merged";
    bnm = "branch --no-merged";
    ca = "commit --amend";
    cam = "${ca} --message";
    can = "${ca} --no-edit";
    cans = "${can} --gpg-sign";
    cas = "${ca} --gpg-sign";
    casm = "${cas} --message";
    cm = "commit --message";
    cnm = "commit --no-verify --message";
    co = "checkout";
    cob = "${co} -b";
    # FIXME: conflicts with git-extras
    # cp = "cherry-pick";
    cpa = "cherry-pick --abort";
    cpc = "cherry-pick --continue";
    cpm = "cherry-pick -xm1";
    cpx = "cherry-pick -x";
    csm = "commit --gpg-sign --message";
    d = "diff";
    dad = "add";
    dc = "diff --cached";
    ds = "diff --stat";
    # ffco = "flow feature checkout";
    # ffr = "flow feature rebase";
    # ffs = "flow feature start";
    # frf = "flow release finish";
    # frfs = "flow release finish -s";
    # frs = "flow release start";
    r = "reset";
    rb = "rebase";
    rba = "rebase --abort";
    rbc = "rebase --continue";
    rbi = "rebase --interactive";
    rbs = "rebase --skip";
    rest = "reset";
    rh = "reset --hard";
    sa = "stash apply";
    sk = "stash --keep-index";
    sl = "stash list";
    sp = "stash pop";
    spa = "stash --patch";
    ss = "stash save";
    st = "status --short";
    stu = "status --short --untracked-files=no";
    stat = "status";
    tree = "log --all --graph --oneline";
  };

}
