{ pkgs, ... }:

{

  environment = {

    pathsToLink = [
      "/share/gap"
    ];

    systemPackages = with pkgs; ([
      gap
      graphviz
    ] ++ (with python27Packages; [
      pygments
      gap-pygments-lexer
    ]));

  };

}
