# -*- mode: perl -*-

$clean_ext .= ' %R.ist %R.xdy';
$pdflatex = q/xelatex %O -interaction=nonstopmode -shell-escape %S/;

add_cus_dep('glo', 'gls', 0, 'run_makeglossaries');
add_cus_dep('acn', 'acr', 0, 'run_makeglossaries');

sub run_makeglossaries {
  $dir = dirname($_[0]);
  $file = basename($_[0]);
  if ( $silent ) {
    system "makeglossaries -q -d '$dir' '$file'";
  }
  else {
    system "makeglossaries -d '$dir' '$file'";
  };
}

push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
push @generated_exts, 'tdo';
