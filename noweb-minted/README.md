# noweb-minted
A simple filter for pretty printing noweb code chunks using the LaTeX minted package

## Requirements

* The ```minted``` LaTeX package
* Pygments. So far, the code is tested on version 2.0.2, under Python 2.7.
* The ```gawk```executable. I installed this using homebrew on OSX.

## Installation
Put the ```noweb-minted```and ```guessPygmentizeLexer.py``` executables into your PATH. For instance:

```
ln -s /path/to/noweb-minted/noweb-minted /usr/local/bin
ln -s /path/to/noweb-minted/guessPygmentizeLexer.py /usr/local/bin
```

## Usage

Use noweb as usual, but now with the ```noweb-minted``` filter. For instance, to generate the```sample.tex``` do the following:

```
> noweave -filter noweb-minted -delay -latex sample.nw > sample.tex
> pdflatex --shell-escape sample.tex
```

The output should be the same as in ```sample.pdf```.




