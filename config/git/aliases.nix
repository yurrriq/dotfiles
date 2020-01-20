{ ... }:

{

  programs.git.aliases = rec {
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

}
