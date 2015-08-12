# Proglr: GLR Parser Generator for Standard ML

## Quick Start

### SML/NJ

```
$ ml-build proglr.cm Main.main proglr
$ cd example/calc
$ sml @SMLload=../../proglr -m smlnj -o parse.sml -l scan.ulex calc.cf
$ ml-build main.cm Main.main main
$ sml @SMLload=main sample.txt
```

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

### Alice ML

Assuming you have compiled proglr by MLton or Poly/ML,
and installed Alice ML and ml-ulex, to run calc example, type:

```
$ cd example/calc
$ ../../proglr -m alice -o parse.sml -l scan.ulex calc.cf
$ make -f Makefile.alice
$ alicerun main sample.txt
```

### MLKit

Assuming you have compiled proglr by MLton or Poly/ML,
and installed mlkit and ml-ulex, to run calc example, type:

```
$ cd example/calc
$ ../../proglr -m mlkit -o parse.sml -l scan.ulex calc.cf
$ mlkit -output main main.mlb
$ ./main sample.txt
```

### Moscow ML

Assuming you have compiled proglr by MLton or Poly/ML,
and installed mosml and ml-ulex, to run calc example, type:

```
$ cd example/calc
$ ../../proglr -m mosml -o parse.sml -l scan.ulex calc.cf
$ make -f Makefile.mosml depend
$ make -f Makefile.mosml
$ ./main sample.txt
```
