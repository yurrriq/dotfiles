{ ... }:

{

  programs.fish.shellAliases = {
    j = "z";
    k = "clear";
    l = "eza --color=auto -G";
    ll = "eza --color=auto -la";
    pbcopy = "xclip -sel clipboard";
    pbpaste = "xclip -sel clipboard -o";
  };

}
