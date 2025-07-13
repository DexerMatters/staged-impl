# staged-impl

This is the implementation of the staged computation system introduced in [A Modal Analysis of Staged Computation](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf)

## Example

```haskell
let add =
  fix f: (Nat, Nat) -> Nat.
  fun n: (Nat, Nat).
    case fst(n) of
        0    => snd(n)
      | s ns => f( (ns, s(snd(n))) )
in
let times = 
  fix p: Nat -> [[Nat -> Nat]].
  fun n: Nat.
    case n of
      0   => [[fun x:Nat. 0]]
    | s m => [[fun x:Nat. add(
                (x, (eval 1 (p(m)))(x))
              )]]
in 
[[(eval 1 (times(2)))(3)]]
```

Codes embraced with `[[]]` are relatively `later` to be evaluated. And `eval n E` evaluates `E` by n stages.