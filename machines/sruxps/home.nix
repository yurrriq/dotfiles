{ lib, pkgs, ... }:
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
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/nixpkgs
    ../../config/password-store.nix
    ../../config/rebar3.nix
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

  home.file."src/gitlab.sportradar.ag/.envrc".text = ''
    case $(kubectl config current-context) in
        *k8s.srus*)
            export AWS_PROFILE=msp-admin
            ;;
        *) ;;
    esac

    export CI_SERVER_HOST=gitlab.sportradar.ag
    export CI_REGISTRY="$CI_SERVER_HOST":4567 \
           CI_REGISTRY_USER=gitlab-ci-token \
           CI_REGISTRY_PASSWORD=$(pass "$CI_SERVER_HOST"/token/registry) \
           CI_JOB_TOKEN=$(pass "$CI_SERVER_HOST"/token/api)
    export GITLAB_TOKEN="$CI_JOB_TOKEN" \
           GITLAB_REGISTRY_ACCESS_TOKEN="$CI_REGISTRY_PASSWORD"
  '';

  home.packages = with pkgs; (
    [
      aws-iam-authenticator
      ec2instanceconnectcli
      progress
      docker-compose
      iw
      jdk
      kubectx
      kubelogin
      k9s
      stern
      # FIXME: naal
      renderizer
      scc
      powertop
      zoom-us
    ] ++ (
      with nodePackages; [
        aws-azure-login
      ]
    )
  );

  _module.args.pkgs = lib.mkForce pkgs;

}
