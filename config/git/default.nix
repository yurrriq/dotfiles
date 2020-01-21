{ config, lib, pkgs, ... }:

{
  imports = [
    ./aliases.nix
    ./config.nix
    ./packages.nix
  ];

  programs.git = {
    enable = true;
    ignores = [
      "*~"
      ".DS_Store"
    ];
    lfs.enable = true;
    userName = "Eric Bailey";
  } // (if config.home.username == "e.bailey" then {
    signing.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";
    userEmail = "e.bailey@sportradar.com";
  } else {
    signing.key = "0CBFCE8BC161F36E2E156A526E5EAB98B1F77A17";
    userEmail = "eric@ericb.me";
  });

}
