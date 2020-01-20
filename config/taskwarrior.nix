{ pkgs, ... }:

{

  home.packages = with pkgs; [
    tasknc
  ];

  programs.taskwarrior = {
    enable = true;
    colorTheme = "solarized-dark-256";
    config = {
      context.work = "jiraurl.any or +work";
      context.other = "jiraurl.none or -work";
      # context = "work";
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

  xdg.dataFile.taskwarrior-on-exit-git = {
    executable = true;
    target = "task/hooks/on-exit-git.sh";
    text = ''
      #! /usr/bin/env bash

      set -eufo pipefail
      if [ -n "''${DEBUG:-}" ]; then
          set -x
      fi
      args="''${2#args:}"
      command="''${3#command:}"
      data="''${5#data:}"

      if git -C "$data" diff --quiet; then
          if [ -n "''${DEBUG:-}" ]; then
              echo 'No changes to commit'
          fi
          exit 0
      elif ! git -C "$data" add -A; then
          echo 'Failed to add files to the index'
          exit 100
      elif ! git -C "$data" commit -qm "$command: ''${args#task $command}"; then
          echo 'Failed to record changes to the repository'
          exit 101
      elif [ -n "''${DEBUG:-}" ]; then
          git -C "$data" log --oneline -1
      fi
    '';
  };

}
