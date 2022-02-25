# Context free grammar to Chomsky normal form

Program to convert context free grammar(CFG) to Chomsky normal form(CNF), 
based on algorithms 4.5 and 4.7 in [TIN Study text](https://www.fit.vutbr.cz/study/courses/TIN/public/Texty/TIN-studijni-text.pdf).

## Setup
Simple Makefile is provided. To simply compile the program run the following command.
```bash
make
```

## Runtime
After successful compilation, program can be run as follows:
```bash
$ ./flp21-fun (-i|-1|-2) [input]
```
If `input` is not specified, the program reads stdin. Option `-i` parse the input to internal representation
and dump it to the stdout. `-1` process simple rules removal. `-2` normalized grammar to the Chomsky normal form.
Both `-1` and `-2` also dumps grammar to the stdout.

## Tests
Several tests are provided in the /tests directory, they test invalid program arguments, invalid inputs, and program functionality on simple inputs. Automatic Python scripts are attached in the zip archive.

Run all tests:
```bash
make test
```


