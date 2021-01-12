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
    # TODO: daemonNiceLevel = 19;
    extraOptions = ''
      experimental-features = flakes nix-command
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    optimise.automatic = true;
    package = pkgs.nixUnstable;
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
    ];
  };

  programs.bash.enableCompletion = true;

}
