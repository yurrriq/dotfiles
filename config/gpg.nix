{ gpgKey, ... }:

{

  programs.gpg = {
    enable = true;
    settings.default-key = gpgKey;
    settings.no-emit-version = true;
  };

}
