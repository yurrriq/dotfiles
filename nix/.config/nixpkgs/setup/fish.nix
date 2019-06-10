{ lib, pkgs, ... }:

{

  environment = {

    systemPackages = [
      pkgs.fish
    ];

    variables = {
      SHELL = "${pkgs.fish}/bin/fish";
      # TERMINAL = "pkgs.kitty/bin/kitty";
    };

  };

  programs.fish = let inherit (lib.strings) fileContents; in {
    enable = true;
    interactiveShellInit = fileContents ./fish/interactiveShellInit.fish;
    promptInit = ''
      ${fileContents ./fish/sushi/fish_prompt.fish}

      ${fileContents ./fish/sushi/fish_right_prompt.fish}
    '';
    shellInit = fileContents ./fish/shellInit.fish;
    vendor.completions.enable = true;
  };

}
