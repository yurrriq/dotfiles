{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    (pass.override {
      tombPluginSupport = true;
    })
  ];

  nixpkgs.config.packageOverrides = super: {
    browserpass = super.callPackage ../pkgs/tools/security/browserpass {};
  };

  programs.browserpass.enable = true;
}
