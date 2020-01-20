self: super: {
  xvfb_run =
    let
      xvfb_run = super.fetchurl {
        url = https://projects.archlinux.org/svntogit/packages.git/plain/trunk/xvfb-run?h=packages/xorg-server;
        sha256 = "1307mz4nr8ga3qz73i8hbcdphky75rq8lrvfk2zm4kmv6pkbk611";
      };
    in
    super.xvfb_run.overrideAttrs(oldAttrs: {
      buildCommand = ''
        mkdir -p $out/bin
        cp ${xvfb_run} $out/bin/xvfb-run
        chmod a+x $out/bin/xvfb-run
        patchShebangs $out/bin/xvfb-run
        wrapProgram $out/bin/xvfb-run \
            --set FONTCONFIG_FILE "${super.texFunctions.fontsConf}" \
            --prefix PATH : ${super.stdenv.lib.makeBinPath (with super; [ getopt xorg.xorgserver xorg.xauth which utillinux gawk coreutils ])}
      '';
    });
}
