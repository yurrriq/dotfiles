{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    # polyml
  ];

}
