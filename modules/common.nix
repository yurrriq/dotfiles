{ pkgs, ... }:

{

  nix = {
    binaryCaches = [
      # TODO: "https://sportradar.cachix.org"
      "https://yurrriq.cachix.org"
    ];
    binaryCachePublicKeys = [
      # TODO: "sportradar.cachix.org-1:TODO"
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
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
