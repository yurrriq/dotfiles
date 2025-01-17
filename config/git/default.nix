{ ... }:

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
  };

}
