{ config, lib, ... }:

let

  airportCode = config.airportCode;

in

{

  options.airportCode = lib.mkOption {
    default = "MSP";
    type = lib.types.enum [ "ATL" /* "LHR" */ "LJU" "MSP" "TRD" ];
  };

  config = lib.mkMerge [
    (lib.mkIf (airportCode == "MSP") {
      location = {
        latitude = 44.93;
        longitude = -93.24;
      };
      time.timeZone = "America/Chicago";
    })
    (lib.mkIf (airportCode == "ATL") {
      location = {
        latitude = 33.76;
        longitude = -84.3;
      };
      time.timeZone = "America/New_York";
    })
    # (lib.mkIf (airportCode == "LHR") {
    #   time.timeZone = "Europe/London";
    # })
    (lib.mkIf (airportCode == "LJU") {
      location = {
        latitude = 46.09;
        longitude = 14.55;
      };
      time.timeZone = "Europe/Ljubljana";
    })
    (lib.mkIf (airportCode == "TRD") {
      location = {
        latitude = 63.43;
        longitude = 10.40;
      };
      time.timeZone = "Europe/Oslo";
    })
  ];

}
