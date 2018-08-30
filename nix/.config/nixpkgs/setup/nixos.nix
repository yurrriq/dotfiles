{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    (pass.override {
      tombPluginSupport = true;
    })
  ];

  programs.browserpass.enable = true;

  services.keybase.enable = true;
}
