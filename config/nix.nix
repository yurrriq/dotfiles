{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;

    settings = {
      experimental-features = [
        "flakes"
        "nix-command"
      ];

      substituters = [
        "https://cache.nixos.org"
        "https://devenv.nixos.org"
        "https://sportradar.cachix.org"
        "https://yurrriq.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "sportradar.cachix.org-1:6MyCzOfUMeMTxU5QnogkyYOBtr5f5atW/qeS+TjmUfE="
        "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      ];
      trusted-users = [
        "root"
        config.home.username
      ];
    };
  };
}
