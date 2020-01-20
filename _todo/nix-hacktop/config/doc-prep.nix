{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # asciidoc
    # docbook5
    # docbook5_xsl
    # ghostscript
    # groff
    # iosevka
    # latex2html
  ];

}
