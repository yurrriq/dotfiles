{ pkgs, ... }:

{

  nix = {
    binaryCaches = [
      "https://yurrriq.cachix.org"
      "https://sportradar.cachix.org" # TODO
    ];
    binaryCachePublicKeys = [
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      # "sportradar.cachix.org-1:FIXME"
    ];
    # TODO: daemonNiceLevel = 19;
    extraOptions = ''
      experimental-features = flakes nix-command
    '';
    gc = {
      automatic = false; # TODO
      options = "--delete-older-than 30d";
    };
    optimise.automatic = false; # TODO
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
