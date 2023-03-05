{ config, ... }:

{
  imports = [
    ./aliases.nix
    ./config.nix
    ./lab.nix
    ./packages.nix
  ];

  programs.git = {
    enable = true;
    ignores = [
      "*~"
      ".DS_Store"
    ];
  } // (
    with config.accounts.email.accounts.primary; {
      userName = realName;
    }
  );

}
