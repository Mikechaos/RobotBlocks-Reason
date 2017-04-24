# Robot Block in Facebook Reason #

This is an implementation of [A simple robot block challenge](https://uva.onlinejudge.org/external/1/101.pdf
)  
It is written in [Facebook Reason](https://facebook.github.io/reason/), a syntax improvement and toolchain sitting on top of Ocaml.

## Install ##

Install steps to be added...

## Example ##
Sample Input
```
let program = [
  "15",
  "move 0 onto 1",
  "move 2 over 0",
  "move 1 onto 4",
  "move 7 over 0",
  "move 5 onto 4",
  "move 3 over 2",
  "move 6 over 1",
  "move 9 over 0",
  "move 14 over 0",
  "move 12 over 0",
  "move 11 over 0",
  "move 13 over 0",
  "move 10 over 0",
  "quit"
];
```
Sample output
```
0: 0 7 9 14 12 11 13 10
1: 1 6
2: 2 3
3: 
4: 4 5
5: 
6: 
7: 
8: 8
9: 
10: 
11: 
12: 
13: 
14:
```

## TODO ##
  * Add install and run steps in this readme
  * Implement `pileOnto` and `pileOver` commands
  * Separate modules in different files
  * Refactor the `ActionHelpers`
  * Improve documentation
