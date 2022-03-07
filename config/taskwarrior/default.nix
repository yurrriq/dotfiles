{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    timewarrior
  ];

  home.sessionVariables = {
    TASKRC = "~/.taskrc-dirty";
  };

  programs.fish.shellAliases = lib.mkIf (config.programs.fish.enable) rec {
    p = "task ls limit:page";
    po = "timew summary :week";
    pp = tbd;
    t = "task limit:page";
    ta = "task add";
    tbd = "task burndown.daily";
    te = "env VISUAL=$EDITOR task edit";
    tl = "task list";
    tm = "task mod";
    tw = "timew";
  };

  programs.taskwarrior = {
    enable = true;
    colorTheme = "solarized-dark-256";
    config = {
      context.other = "-work";
      context.work = "+work";
    };
  };

  xdg.dataFile."task/hooks/on-exit-git.sh" = {
    executable = true;
    source = ./on-exit-git.sh;
  };

  xdg.dataFile."task/hooks/on-modify.timewarrior" = {
    executable = true;
    source = let inherit (pkgs.timewarrior) version; in
      with pkgs; stdenv.mkDerivation {
        pname = "taskwarrior-on-modify.timewarrior";
        inherit version;
        nativeBuildInputs = [ makeWrapper ];
        buildInputs = [ python3 ];
        src = fetchurl {
          url = "https://raw.githubusercontent.com/GothenburgBitFactory/timewarrior/v${version}/ext/on-modify.timewarrior";
          sha512 = "sha512-GsDqetyfQOUU+VTZbgdKH1X6n5tM7q3Q0B5X/zk+JHgzw6vVk48IxGvCaDnpIJXCASIgSGsKLxLvv7RPDdlPAw==";
        };
        dontUnpack = true;
        installPhase = ''
          install -m755 $src $out
          substituteInPlace $out --replace "/usr/bin/env " ${python3}/bin/
        '';
      };
  };

}
