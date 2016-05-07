#_F_<sub>n</sub> scala

`Fn.scala` is a simple experimental tool for computing and listing Fibonacci numbers.
Its capabilities are generating a list of subsequent Fibonacci numbers by their ordinal positions or just putting the given 
exponential position of each **_F_<sub>n</sub>** in sequence to print some amazing Fibonacci effects.

## Some Examples
```bash
$ ./Fn.scala -le12
$ ./Fn.scala -n10 -e200 -w60
$ ./Fn.scala -p4 -b02 -r3w89
$ ./Fn.scala -n10 -r6 -e200 -w48
```


## Available Properties
```
Fn.pos       0
Fn.rad      10
Fn.lineWidth 0
Fn.lister    false
Fn.debug     false
Fn.quiet     false
Fn.usage     false  
Fn.blackDigs  
Fn.calculate false
Fn.iterate   <!Fn.calculate>
Fn.DIG_B     \u00A0
Fn.DIG_W     \u2022
```