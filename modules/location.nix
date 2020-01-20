{ lib, airportCode ? "MSP" }:

let
  mkLocation = locations:
    assert (lib.assertMsg (lib.hasAttr airportCode locations)
      "Unknown location: ${airportCode}");
    lib.getAttr airportCode locations;
in

mkLocation {
  "ATL" = {
    location = {
      latitude = 33.76;
      longitude = -84.3;
    };
    time.timeZone = "America/New_York";
  };
  "LHR" = {
    time.timeZone = "Europe/London";
  };
  "LJU" = {
    location = {
      latitude = 46.09;
      longitude = 14.55;
    };
    time.timeZone = "Europe/Ljubljana";
  };
  "MSP" = {
    location = {
      latitude = 44.93;
      longitude = -93.24;
    };
    time.timeZone = "America/Chicago";
  };
  "TRD" = {
    time.timeZone = "Europe/Oslo";
  };
}
