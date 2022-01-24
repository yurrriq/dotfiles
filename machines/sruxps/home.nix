{ pkgs, ... }:
{
  imports = [
    ../../config/bash.nix
    ../../config/bat.nix
    ../../config/browserpass.nix
    ../../config/bugwarrior.nix
    ../../config/direnv.nix
    ../../config/dunst.nix
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/jq.nix
    ../../config/keyboard.nix
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/password-store.nix
    ../../config/rebar3.nix
    ../../config/screen-locker.nix
    ../../config/starship.nix
    ../../config/taskwarrior
    ../../config/xmonad
  ];
  accounts.email.accounts.primary = {
    address = "e.bailey@sportradar.com";
    gpg.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";
    primary = true;
    realName = "Eric Bailey";
  };
  home.file.".docker/config.json".text = ''
    {
        "credHelpers": {
            "docker.io": "pass",
            "gitlab.sportradar.ag:4567": "pass"
        }
    }
  '';
  home.file."src/gitlab.sportradar.ag/.envrc".text = ''
    case $(kubectl config current-context) in
        *k8s.srus*|sapi*nov1*)
            export AWS_PROFILE=msp
            ;;
        *)
            export AWS_PROFILE=default
    esac

    export CI_SERVER_HOST=gitlab.sportradar.ag
    export CI_JOB_TOKEN=$(pass "$CI_SERVER_HOST"/token/api)
    export CI_REGISTRY="$CI_SERVER_HOST":4567 \
           CI_REGISTRY_USER=gitlab-ci-token \
           CI_REGISTRY_PASSWORD="$CI_JOB_TOKEN" \
           GITLAB_TOKEN="$CI_JOB_TOKEN"
    export GITLAB_REGISTRY_ACCESS_TOKEN="$CI_REGISTRY_PASSWORD"
  '';
  home.keyboard = {
    options = [
      "ctrl:nocaps"
      "compose:ralt"
    ];
  };
  home.packages = with pkgs; [
    aws-iam-authenticator
    awscli2
    ec2instanceconnectcli
    nodePackages.aws-azure-login
    naal
    bpytop
    fd
    powertop
    progress
    renderizer
    scc
    docker-credential-helpers
    kubectx
    stern
    zoom-us
    # super-productivity
  ];
  services.picom = {
    experimentalBackends = true;
    extraOptions = ''
      unredir-if-possible = true;
    '';
    vSync = true;
  };
}
