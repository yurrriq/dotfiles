{ lib, pkgs, ... }:

{

  environment = {

    loginShell = "${pkgs.fish}/bin/fish";

    systemPackages = [
      pkgs.fish
    ];

  };

  programs.fish = let inherit (lib.strings) fileContents; in {
    enable = true;
    interactiveShellInit = fileContents ./fish/interactiveShellInit.fish;
    shellInit = fileContents ./fish/shellInit.fish;
  };

}
