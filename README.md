# fsharp-vectors

Type-safe vectors in F#.

# What?

```fsharp
let l1 = [ 1 ; 2 ]
let l2 = [ 3 ]
List.zip l1 l2
|> ignore
```

Oh no! Lists aren't safe!

```fsharp
let v1 = 1 ** 2 ** -()
let v2 = 1 ** ()
Vector.zip v1 v2 // doesn't compile!
|> ignore
```

# How?

Peano encoding of the naturals, basically.
We define a type `Z` and a type `S : * -> *`, and interpret `S Z` as the number 1.

# Syntax

I refuse to apologise for the syntax.

* `Vector.empty` can also be written `-()`. (It's actually an overload of the unary `-`.)
* `Vector.cons` can also be written `**`. (It had to be this way because we need right-associativity, and not that many symbols in F# have right-associativity.)

# Efficiency

These things are actually not that bad at runtime, although on no account should you use `VectorCrate` on the hot path.
I have yet to do any benchmarking, but I think they'll be pretty good.
