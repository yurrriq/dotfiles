self: super:

{

  python35Packages = super.python35Packages // {
    bugwarrior = super.python35Packages.callPackage ./bugwarrior {};
  };

}
