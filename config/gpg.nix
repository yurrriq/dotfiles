{ config, ... }:

{

  programs.gpg = {
    enable = true;
    settings = {
      default-key = config.accounts.email.accounts.primary.gpg.key;
      keyid-format = "long";
      no-emit-version = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    enableSshSupport = true;
    maxCacheTtl = 3600;
  };

}
