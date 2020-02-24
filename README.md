# noweb-minted

A simple filter for pretty printing [noweb][1] code chunks,
using [the `minted` LaTeX package][2]


## Requirements

* [The `minted` LaTeX package][2]
* [`noweb.sty`][10] (comes with a [noweb][1])
* [Pygments][3] (tested on version 2.4.2, under Python 3.7)
* [gawk][4]
* *Optional:* [GNU Make][9]

## Installation

Put the [`noweb-minted`][5] and [`guessPygmentizeLexer.py`][6] executables onto
your `$PATH`, e.g.

```sh
ln -s "$PWD/noweb-minted" /usr/local/bin
ln -s "$PWD/guessPygmentizeLexer.py" /usr/local/bin
```

## Usage

Use [noweb][1] as usual, but now with the [`noweb-minted`][5] filter.

For instance, to generate [`sample.tex`][7], run `make sample.tex`, i.e.

```sh
noweave -filter noweb-minted -delay -latex sample.nw >sample.tex
```

To generate [`sample.pdf`][8], run `make sample.pdf`, i.e.

```sh
pdflatex --shell-escape sample.tex
```




<!-- Links -->
[1]: https://www.cs.tufts.edu/~nr/noweb/
[2]: https://www.ctan.org/pkg/minted
[3]: https://pygments.org/
[4]: https://www.gnu.org/software/gawk/
[5]: https://github.com/yurrriq/noweb-minted/blob/master/noweb-minted
[6]: https://github.com/yurrriq/noweb-minted/blob/master/guessPygmentizeLexer.py
[7]: https://github.com/yurrriq/noweb-minted/blob/master/sample.tex
[8]: https://raw.githubusercontent.com/yurrriq/noweb-minted/master/sample.pdf
[9]: https://www.gnu.org/software/make/
[10]: https://github.com/nrnrnr/noweb/blob/v2_12/src/tex/noweb.sty
