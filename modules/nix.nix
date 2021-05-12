{ pkgs, ... }:
{
  nix = {
    binaryCaches = [
      "https://yurrriq.cachix.org"
    ];
    binaryCachePublicKeys = [
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
    ];
    # TODO: daemonNiceLevel = 19;
    extraOptions = ''
      experimental-features = ca-references flakes nix-command
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    optimise.automatic = true;
    package = pkgs.nixUnstable;
  };
  environment.systemPackages = with pkgs; [
    cachix
  ];
}
