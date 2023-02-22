{ config, pkgs, ... }:
{
  imports = [
    ../../config/applications.nix
    ../../config/bash.nix
    ../../config/bat.nix
    ../../config/browserpass.nix
    # ../../config/bugwarrior.nix
    ../../config/clis.nix
    ../../config/direnv.nix
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/git/lab.nix
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/jq.nix
    ../../config/keyboard.nix
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/nix.nix
    ../../config/password-store.nix
    ../../config/rebar3.nix
    ../../config/screen-locker.nix
    ../../config/starship.nix
    # ../../config/taskwarrior
    ../../config/xmonad
  ];
  accounts.email.accounts.primary = {
    address = "e.bailey@sportradar.com";
    gpg.key = "86BAD22D1F8DBBEC486C49012C32D5C1C17A8045";
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
           GITLAB_TOKEN="$CI_JOB_TOKEN" \
           JIRA_API_TOKEN="$(pass jira.sportradar.ag/e.bailey)" \
           JIRA_AUTH_TYPE=bearer
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
    bpytop
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
    lens
    stern
    vault
    fortune
    prometheus-alertmanager
    bind
    curl
    httpie
    cachix
    nixUnstable
    home-manager
    nixgl.auto.nixGLDefault
    networkmanager
  ];
  services.picom = {
    settings = {
      unredir-if-possible = true;
    };
    vSync = true;
  };
  home.sessionPath = [
    "${config.home.homeDirectory}/bin"
  ];

  programs.kitty.settings.font_size = 12;

  services.random-background = {
    enable = true;
    imageDirectory = "/usr/share/backgrounds/";
    display = "scale";
  };

  xresources.properties = {
    "Xft.dpi" = 220;
  };
}
