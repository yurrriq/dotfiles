{ lib, pkgs, ... }:

{

  environment = {

    systemPackages = [
      pkgs.fish
    ];

    variables = {
      SHELL = "${pkgs.fish}/bin/fish";
    };

  };

  programs.fish = let inherit (lib.strings) fileContents; in {
    enable = true;
    interactiveShellInit = fileContents ./fish/interactiveShellInit.fish;
    # shellInit = fileContents ./fish/shellInit.fish;
    vendor.completions.enable = true;
  };

}
