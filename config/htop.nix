{ ... }:

{

  programs.htop = {
    enable = true;
    settings = {
      color_scheme = 6;
      cpu_count_from_zero = true;
      highlight_base_name = true;
      show_cpu_usage = true;
    };
  };

}
