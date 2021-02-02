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
    pp = tbd;
    t = "task limit:page";
    ta = "task add";
    tbd = "task burndown.daily";
    te = "env VISUAL=$EDITOR task edit";
    tl = "task list";
    tm = "task mod";
  };

  programs.taskwarrior = {
    enable = true;
    colorTheme = "solarized-dark-256";
    config = {
      context.other = "jiraurl.none or -work";
      context.work = "jiraurl.any or +work";
      uda = {
        jiracreatedts = {
          label = "Created At";
          type = "date";
        };
        jiradescription = {
          label = "Jira Description";
          type = "string";
        };
        jiraestimate = {
          label = "Estimate";
          type = "numeric";
        };
        jirafixversion = {
          label = "Fix Version";
          type = "string";
        };
        jiraid = {
          label = "Jira Issue ID";
          type = "string";
        };
        jiraissuetype = {
          label = "Issue Type";
          type = "string";
        };
        jirastatus = {
          label = "Jira Status";
          type = "string";
        };
        jirasummary = {
          label = "Jira Summary";
          type = "string";
        };
        jiraurl = {
          label = "Jira URL";
          type = "string";
        };
      };
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
        src = fetchurl {
          url = "https://raw.githubusercontent.com/GothenburgBitFactory/timewarrior/v${version}/ext/on-modify.timewarrior";
          sha512 = "sha512-CaaELr0JaDJ/LNvwUGDQUQPoKEOapGkJMycD9ujv0DWgfTQzoYkw/JCDPChreT4xCEJ303sAx4NpUqb+260orw==";
        };
        dontUnpack = true;
        installPhase = ''
          cp $src $out
        '';
        postInstall = ''
          wrapProgram $out --prefix PATH : ${lib.makeBinPath [ python3 ]}
        '';
      };
  };

}
