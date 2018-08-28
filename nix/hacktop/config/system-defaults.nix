{ ... }:

{

  # TODO: dig through https://github.com/mathiasbynens/dotfiles/blob/master/.macos
  system.defaults = {

    NSGlobalDomain = {
      # Enable full keyboard access, e.g. tab in dialogs
      AppleKeyboardUIMode = 3;

      # Disable press-and-hold in favor of key repeat
      ApplePressAndHoldEnabled = false;
      InitialKeyRepeat = 10;
      KeyRepeat = 1;

      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = true;

      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };

    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "right";
      showhidden = true;
    };

    finder = {
      AppleShowAllExtensions = true;
      FXEnableExtensionChangeWarning = false;
      QuitMenuItem = true;
    };

    trackpad.Clicking = true;

  };

}
