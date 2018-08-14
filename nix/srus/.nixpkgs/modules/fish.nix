{ lib, pkgs, ... }:

{

  environment = {

    loginShell = "${pkgs.fish}/bin/fish";

    systemPackages = [
      pkgs.fish
    ];

  };

  programs.fish = {
    enable = true;
    interactiveShellInit = lib.strings.fileContents ./fish/interactiveShellInit.fish;
    shellInit = lib.strings.fileContents ./fish/shellInit.fish;
  };

}
