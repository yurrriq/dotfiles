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
    signing.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";
    userEmail = "e.bailey@sportradar.com";
    userName = "Eric Bailey";
  };

}
