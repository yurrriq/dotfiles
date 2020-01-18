{ pkgs, ... }:

{

  programs.firefox = {
    enable = true;
    extensions = with (import <nur> { inherit pkgs; }).repos.rycee.firefox-addons; [
      https-everywhere
      privacy-badger
    ];
  };

}
