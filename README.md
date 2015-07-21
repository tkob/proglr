# proglr

Proglr: GLR Parser Generator for Standard ML

## Quick Start

Assuming you have mlton and ml-ulex installed, to run calc example, type:

```
$ make -f Makefile.mlton
$ cd example/calc
$ ../../proglr -m mlton -o parse.sml -l scan.ulex calc.cf
$ mlton main.mlb
$ ./main sample.txt
```
