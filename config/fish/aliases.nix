{ ... }:

{

  programs.fish.shellAliases = {
    k = "clear";
    l = "exa --color=auto -G";
    ll = "exa --color=auto -Gla";
    pbcopy = "xclip -sel clipboard";
    pbpaste = "xclip -sel clipboard -o";
  };

}
