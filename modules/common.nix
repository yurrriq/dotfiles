{ pkgs, ... }:

{

  nix = {
    binaryCaches = [
      "https://yurrriq.cachix.org"
      "https://yurrriq-nur-packages.cachix.org"
    ];
    binaryCachePublicKeys = [
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      "yurrriq-nur-packages.cachix.org-1:7kbjuGBUZcWf876g2cdelmIQXrXzOhpMVBqYOyyAv70="
    ];
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      (nerdfonts.override { withFont = "Iosevka"; })
    ];
  };

  programs.bash.enableCompletion = true;

}
