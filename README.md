# proglr

Proglr: GLR Parser Generator for Standard ML

## Quick Start

### MLton

Assuming you have mlton and ml-ulex installed, to run calc example, type:

```
$ make -f Makefile.mlton
$ cd example/calc
$ ../../proglr -m mlton -o parse.sml -l scan.ulex calc.cf
$ mlton main.mlb
$ ./main sample.txt
```

### Poly/ML

Assuming you have poly and ml-ulex installed, to run calc example, type:

```
$ make -f Makefile.poly
$ cd example/calc
$ ../../proglr -m poly -o parse.sml -l scan.ulex calc.cf
$ make -f Makefile.poly
$ ./main sample.txt
```
