namespace Vector.Naturals

/// Zero, in the Peano encoding of the naturals
type Z =
    private
    | Z

/// Successor, in the Peano encoding of the naturals
type S<'a> =
    private
    | S
    // Computation rules
    static member inline refine< ^n1, ^n2 when ^n1 : (static member refine : ^n1 -> ^n2)> (_ : S<'n1>) : S<'n2> = Unchecked.defaultof<_>

/// Type representing the sum of two naturals
type Add<'a, 'b> =
    private
    | Add
    // Computation rules
    static member inline refine (_ : Add<Z, Z>) : Z = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2> (_ : Add< S< ^n1>, S< ^n2>>) : S<S<Add< ^n1, ^n2>>> = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2> (_ : Add< Z, ^n1>) : ^n2 = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2 > (_ : Add< ^n1, Z>) : ^n2 = Unchecked.defaultof<_>

