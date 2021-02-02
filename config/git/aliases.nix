{ config, lib, ... }:

{

  programs.fish.shellAbbrs = lib.mkIf (config.programs.fish.enable) {
    g = "git";
    gd = "git d";
    gdc = "git dc";
    gm = "git merge";
    gs = "git st";
    gt = "git tree";
  };

  programs.git.aliases = rec {
    ap = "add -p";
    bm = "branch --merged";
    bnm = "branch --no-merged";
    ca = "commit --amend";
    cam = "${ca} -m";
    can = "${ca} --no-edit";
    cans = "${can} -S";
    cas = "${ca} -S";
    casm = "${cas} -m";
    cm = "commit -m";
    cnm = "commit -nm";
    co = "checkout";
    cob = "${co} -b";
    cp = "cherry-pick";
    cpa = "${cp} --abort";
    cpc = "${cp} --continue";
    cs = "crypt status";
    cse = "crypt status -e";
    csm = "commit -S -m";
    csu = "crypt status -u";
    d = "diff";
    dad = "add";
    dc = "diff --cached";
    ds = "diff --stat";
    ffco = "flow feature checkout";
    ffr = "flow feature rebase";
    ffs = "flow feature start";
    frf = "flow release finish";
    frfs = "flow release finish -s";
    frs = "flow release start";
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
    st = "status -s";
    stat = "status";
    tree = "log --all --graph --oneline";
  };

}
