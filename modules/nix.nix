{ pkgs, ... }:
{
  nix = {
    settings = {
      substituters = [
        "https://yurrriq.cachix.org"
      ];
      trusted-public-keys = [
        "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      ];
    };
    # TODO: daemonNiceLevel = 19;
    extraOptions = ''
      experimental-features = flakes nix-command
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    optimise.automatic = true;
  };
  environment.systemPackages = with pkgs; [
    cachix
  ];
}
