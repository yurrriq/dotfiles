{ pkgs, ... }:

{
  
  environment.systemPackages = with pkgs; [
    pass
  ];

  nixpkgs.config.packageOverrides = super: {
    browserpass = super.callPackage ../pkgs/tools/security/browserpass {};
  };
    
  programs.browserpass.enable = true;
}
