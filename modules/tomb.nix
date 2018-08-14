{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.programs.tomb;

in


{

  options = {

    programs.tomb = {

      enable = mkOption {
        default = false;
        description = ''
          Whether to configure Tomb.
        '';
        type = types.bool;
      };

      fastEntropy = mkOption {
        default = false;
        description = ''
          Whether to enable fast entropy generation for key forging.
        '';
        types = types.bool;
      };

      qrcode = mkOption {
        default = false;
        description = ''
          Whether to enable engraving keys into printable QR code sheets.
        '';
        types = types.bool;
      };

      resize = mkOption {
        default = false;
        description = ''
          Whether to enable extending the size of existing tomb volumes.
        '';
        types = types.bool;
      };

      searchArchives = mkOption {
        default = false;
        description = ''
          Whether to enable fast searching of contents in compressed archives.
        '';
        types = types.bool;
      };

      searchContents= mkOption {
        default = false;
        description = ''
          Whether to enable fast searching of file contenst inside tombs.
        '';
        types = types.bool;
      };

      searchDocuments = mkOption {
        default = false;
        description = ''
          Whether to enable fast searching of contents in PDF and DOC files.
        '';
        types = types.bool;
      };

      searchNames = mkOption {
        deafult = false;
        description = ''
          Whether to enable fast searching of file names inside tombs.
        '';
        types = types.bool;
      };

      showProgress = mkOption {
        default = false;
        description = ''
          Whether to show progress while digging tombs and keys.
        '';
        types = types.bool;
      };

      slam = mkOption {
        default = false;
        description = ''
          Whether to enable slamming a tomb.
        '';
        types = types.bool;
      };

      steganography = mkOption {
        default = false;
        description = ''
          Whether to enable burying and exhuming keys inside images.
        '';
        types = types.bool;
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; (
      [ file tomb ] ++
      optional cfg.slam lsof ++
      optional cfg.qrcode libqrencode ++
      optional cfg.steganography steghide
    );
  };
}
