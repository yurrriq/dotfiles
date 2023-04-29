{ config, lib, ... }:

{

  programs.gpg = {
    enable = true;
    settings = {
      default-key =
        (builtins.head
          (lib.filter (account: account.primary)
            (lib.attrValues config.accounts.email.accounts))).gpg.key;
      keyid-format = "long";
      no-emit-version = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 28800;
    enableSshSupport = true;
    maxCacheTtl = 28800;
  };

}
