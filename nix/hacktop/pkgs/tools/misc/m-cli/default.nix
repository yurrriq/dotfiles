{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "m-cli-${version}";
  version = "0.2.5";

  src = fetchFromGitHub {
    owner = "rgcr";
    repo = "m-cli";
    rev = "v${version}";
    sha512 = "0mkcx7jq91pbfs8327qc8434gj73fvjgdfdsrza0lyd9wns6jhsqsf0585klzl68aqscvksgzi2asdnim4va35cdkp2fdzl0g3sm4kd";
  };

  dontBuild = true;

  installPhase = ''
    local MPATH=$out/share/m

    gawk -i inplace '{
      gsub(/^\[ -L.*|^\s+\|\| pushd.*|^popd.*/, "");
      gsub(/MPATH=.*/, "MPATH='$MPATH'");
      gsub(/(update|uninstall)_mcli \&\&.*/, "echo NOOP \\&\\& exit 0");
      print
    }' m

    install -dm755 $MPATH/plugins
    install -Dm755 plugins/* $MPATH/plugins/

    install -Dm755 m $out/bin/m

    # TODO: completion/bash/m
    # TODO: completion/zsh/_m
    # TODO: completion/fish/m.fish
  '';

  meta = with stdenv.lib; {
    description = "Swiss Army Knife for macOS";
    inherit (src.meta) homepage;
    repositories.git = git:/https://github.com/rgcr/m-cli.git;

    license = licenses.mit;

    platforms = platforms.darwin;
    maintainers = with maintainers; [ yurrriq ];
  };
}
