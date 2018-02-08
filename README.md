# Functional programming course assignments
My solution to the homework assignments from the functional programming course at Charles University
([NAIL097](https://is.cuni.cz/studium/predmety/index.php?do=predmet&kod=NAIL097), [the course's Github](https://github.com/vituscze/fp)).

## Beta-reduction

The implementation uses De Bruijn indices to represent lambda terms.
HUnit tests may be ran with
```
runghc TestLambda
```

## Hindley-Milner type system -- W algorithm

Implementation of the W algorithm. Defines a function `w` which determines types of terms.
For example, to determine the type of `let x = (λa.a) in x x`:

```
*HM> let (s,t,v2) = w [] (Let 1 (Abs 2 (Var 2)) (App (Var 1) (Var 1))) 100
*HM> t
(α103->α103)
``` 

The resulting type is that of an identity function.

Again, tests may be ran with `runghc TestHM`. To test the unification algorithm, run `runghc TestUnification`.
