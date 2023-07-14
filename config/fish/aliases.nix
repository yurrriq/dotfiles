{ ... }:

{

  programs.fish.shellAliases = {
    j = "z";
    k = "clear";
    l = "exa --color=auto -G";
    ll = "exa --color=auto -la";
    pbcopy = "xclip -sel clipboard";
    pbpaste = "xclip -sel clipboard -o";
  };

}
