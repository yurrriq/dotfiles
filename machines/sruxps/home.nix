{ config, lib, pkgs, ... }:
{
  imports =
    let
      inherit (builtins) any attrNames filter match pathExists readDir toPath;
      inherit (lib.strings) hasSuffix;
      resolveConfig = relativePath:
        toPath (../../config + ("/" + relativePath));
      isConfig = path:
        hasSuffix ".nix" path ||
        pathExists (../../config + ("/" + path + "/default.nix"));
      allConfigs =
        map resolveConfig (filter isConfig (attrNames (readDir ../../config)));
      excludes = [
        ".*bugwarrior\\.nix$"
        ".*taskwarrior$"
      ];
    in
    filter
      (path: !(any (pattern: match pattern path != null) excludes))
      allConfigs;
  accounts.email.accounts.primary = {
    address = "e.bailey@sportradar.com";
    gpg.key = "86BAD22D1F8DBBEC486C49012C32D5C1C17A8045";
    primary = true;
    realName = "Eric Bailey";
  };
  accounts.email.accounts.personal = {
    address = "eric@ericb.me";
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
    nixgl.nixGLIntel
    networkmanager
  ];
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

  programs.kitty.settings.font_size = 12;

  services.random-background = {
    enable = true;
    imageDirectory = "/usr/share/backgrounds/";
    display = "scale";
  };

  targets.genericLinux.enable = true;

  xresources.properties = {
    "Xft.dpi" = 290;
  };
}
