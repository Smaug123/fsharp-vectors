namespace Vector.Naturals

type Dummy =
    private
    | Dummy
    static member inline singleton () = ()

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
    static member inline refine< ^n1, ^n2> (_ : Add< S< ^n1>, ^n2>) : S<Add< ^n1, ^n2>> = Unchecked.defaultof<_>
    //static member inline refine< ^n1, ^n2> (_ : Add< S< ^n1>, S< ^n2>>) : S<S<Add< ^n1, ^n2>>> = Unchecked.defaultof<_>
    static member inline refine< ^n1> (_ : Add< Z, ^n1>) : ^n1 = Unchecked.defaultof<_>

    // We define a hierarchy of operations, making sure to complete all lower operations before attempting a higher one.
    // This is how we guarantee there is at most one operation to perform per computation step.
    static member additionOrHigher () = ()

    static member inline refine< ^n1, ^n2, ^n3 when ^n1 : (static member refine : ^n1 -> ^n3) and ^n1 : (static member additionOrHigher : unit -> unit)>
        (_ : Add< ^n1, ^n2>) : Add< ^n3, ^n2> = Unchecked.defaultof<_>


/// Type representing the product of two naturals
type Times<'x, 'y> =
    private
    | Times
    // Computation rules
    static member inline refine< ^c> (_ : Times<Z, ^c>) : Z = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2>
        (_ : Times<S< ^n1>, ^n2>) : Add< ^n2, Times< ^n1, ^n2>> = Unchecked.defaultof<_>
    static member inline refine< ^a, ^b, ^c> (_ : Times<Add< ^a, ^b>, ^c>) : Add<Times< ^a, ^c>, Times< ^b, ^c>> = Unchecked.defaultof<_>

    // Multiplication is only to be performed after all the additions below are complete...
    static member inline additionOrHigher () = ()

    // ... and also after all the multiplications below are complete.
    static member inline multiplicationOrHigher () = ()

    static member inline refine< ^a, ^b, ^c when ^a : (static member multiplicationOrHigher : unit -> unit) and ^a : (static member refine : ^a -> ^c)>
        (_ : Times< ^a, ^b>) : Times< ^c, ^b> = Unchecked.defaultof<_>
