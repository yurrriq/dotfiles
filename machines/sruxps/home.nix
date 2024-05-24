{ config, lib, pkgs, ... }:
{
  imports = [
    ../../config/screen-locker.nix
  ];
  accounts.email.accounts = {
    personal.address = "eric@ericb.me";
    work = {
      address = "e.bailey@sportradar.com";
      gpg.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";
      primary = true;
      realName = "Eric Bailey";
    };
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
    export CI_REGISTRY="$CI_SERVER_HOST":4567
    export CI_REGISTRY_USER=gitlab-ci-token
    export CI_REGISTRY_PASSWORD="$CI_JOB_TOKEN"
    export GITLAB_TOKEN="$CI_JOB_TOKEN"
    # export JIRA_API_TOKEN="$(pass jira.sportradar.ag/e.bailey)"
    # export JIRA_AUTH_TYPE=bearer
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
    python3Packages.ec2instanceconnectcli
    btop
    fd
    gomplate
    powertop
    progress
    scc
    docker-credential-helpers
    # TODO: podman-compose
    krew
    kubectl
    kubectx
    kubelogin
    kustomize
    # NOTE: unfree license now
    # lens
    stern
    vault
    fortune
    prometheus-alertmanager
    bind
    curl
    httpie
    cachix
    nix
    home-manager
    nixgl.nixGLIntel
    networkmanager
  ];
  home.stateVersion = "23.11";
  services.picom = {
    enable = true;
    settings = {
      unredir-if-possible = true;
    };
    vSync = true;
  };
  home.sessionPath = [
    "${config.home.homeDirectory}/bin"
  ];

  nix.enable = true;

  programs.kitty.settings.font_size = 10;

  services.random-background = {
    enable = true;
    imageDirectory = "/usr/share/backgrounds/";
    display = "scale";
  };

  targets.genericLinux.enable = true;

  xresources.properties = {
    "Xft.dpi" = 290;
  };
  programs.rbw = {
    enable = true;
    settings = {
      email = config.accounts.email.accounts.personal.address;
    };
  };
}
