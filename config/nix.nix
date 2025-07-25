{ config, ... }:

{
  nix = {
    settings = {
      experimental-features = [
        "flakes"
        "nix-command"
      ];

      substituters = [
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
        "https://nix-community.cachix.org"
        "https://numtide.cachix.org"
        "https://pre-commit-hooks.cachix.org"
        "https://sportradar.cachix.org"
        "https://yurrriq.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
        "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
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
