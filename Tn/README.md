#_T_<sub>n</sub> scala

`Tn.scala` is a simple experimental tool for computing and listing triangular numbers.
Its capabilities are generating a list of subsequent triangular numbers by their ordinal positions or just putting the given 
exponential position of each **_T_<sub>n</sub>** in sequence to print some amazing triangular palindromes.

## Some Examples
```bash
$ ./Tn.scala -l
$ ./Tn.scala -n10
$ ./Tn.scala -p1r17n10g -e11g
$ ./Tn.scala -p12r2
$ ./Tn.scala -p12r3n100
$ ./Tn.scala -b13579 -n160010000 -e160013300 -p5
```


## Available Properties
```
Tn.pos       0
Tn.rad      10
Tn.lineWidth 0
Tn.lister    false
Tn.debug     false
Tn.quiet     false
Tn.usage     false
Tn.blackDigs     
Tn.iterate   false
Tn.calculate <!Tn.iterate>      
Tn.DIG_B     \u00A0
Tn.DIG_W     \u2022
```