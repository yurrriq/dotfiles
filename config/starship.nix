{ lib, ... }:
{
  programs.starship = {
    enable = true;
    package = (import <nixpkgs-unstable> { }).starship;
    settings = {
      add_newline = false;
      character = {
        success_symbol = "[λ](bold green)";
        error_symbol = "[λ](bold red)";
      };
      format = builtins.concatStringsSep "" (
        lib.splitString "\n" ''
          $username
          $hostname
          $shlvl
          $kubernetes
          $directory
          $git_branch
          $git_commit
          $git_state
          $git_status
          $hg_branch
          $docker_context
          $package
          $cmake
          $dart
          $dotnet
          $elixir
          $elm
          $erlang
          $golang
          $helm
          $java
          $julia
          $nim
          $nodejs
          $ocaml
          $perl
          $php
          $purescript
          $python
          $ruby
          $rust
          $swift
          $terraform
          $zig
          $nix_shell
          $conda
          $memory_usage
          $aws
          $gcloud
          $openstack
          $env_var
          $crystal
          $cmd_duration
          $custom
          $lua
          $jobs
          $battery
          $time
          $line_break
          $status
          $character
        ''
      );
      git_branch.symbol = "🌱 ";
      git_commit.tag_disabled = false;
      git_status = {
        ahead = ''⇡''${count}'';
        behind = ''⇣''${count}'';
        diverged = ''⇕⇡''${ahead_count}⇣''${behind_count}'';
        staged = "+$count";
      };
      nix_shell = {
        format = "via [$symbol$state( \\($name\\))]($style) ";
        impure_msg = "ι";
        pure_msg = "﻿ρ";
        symbol = "❄️";
      };
      stats.disabled = false;
      time.disabled = false;
    };
  };
}
