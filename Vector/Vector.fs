namespace Vector

open Vector.Naturals

/// A vector with known length.
[<Struct>]
type Vector<'a, 'len> = private | Vector of 'a list

type VectorCrateEvaluator<'a, 'ret> = abstract Eval<'n> : Vector<'a, 'n> -> 'ret
type VectorCrate<'a> = abstract Apply<'ret> : VectorCrateEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module VectorCrate =
    let make<'a, 'n> (v : Vector<'a, 'n>) : VectorCrate<'a> =
        { new VectorCrate<'a> with
            member __.Apply e = e.Eval v
        }

[<RequireQualifiedAccess>]
module Vector =
    let empty<'a> : Vector<'a, Z> = Vector []

    let cons<'a, 'n> (x : 'a) (Vector v : Vector<'a, 'n>) : Vector<'a, S<'n>> =
        Vector (x :: v)

    let toList<'a, 'n> (Vector v : Vector<'a, 'n>) : 'a list = v

    /// This is very inefficient, and I can't imagine it being helpful,
    /// but it's here for completeness.
    let rec ofList<'a> (v : List<'a>) : VectorCrate<'a> =
        match v with
        | [] -> VectorCrate.make empty
        | x :: xs ->
            { new VectorCrate<'a> with
                member __.Apply e =
                    (ofList xs).Apply
                        { new VectorCrateEvaluator<_,_> with
                            member __.Eval v =
                                e.Eval (cons x v)
                        }
            }

    /// Vector.append v1 v2 is v1 ++ v2. (Which is a bit sad, given how often we would want to pipe this,
    /// but it's consistent with List.)
    let append<'a, 'n1, 'n2> (Vector v1 : Vector<'a, 'n1>) (Vector v2 : Vector<'a, 'n2>) : Vector<'a, Add<'n1, 'n2>> =
        List.append v1 v2
        |> Vector

    let fold<'a, 's, 'n> (f : 's -> 'a -> 's) (s : 's) (Vector v : Vector<'a, 'n>) =
        List.fold f s v

    let zip<'a, 'b, 'n> (Vector v1 : Vector<'a, 'n>) (Vector v2 : Vector<'b, 'n>) : Vector<'a * 'b, 'n> =
        List.zip v1 v2
        |> Vector

    let map<'a, 'b, 'n> (f : 'a -> 'b) (Vector v : Vector<'a, 'n>) : Vector<'b, 'n> =
        List.map f v
        |> Vector

    let head<'a, 'n> (Vector v : Vector<'a, S<'n>>) : 'a = List.head v
    let tail<'a, 'n> (Vector v : Vector<'a, S<'n>>) : Vector<'a, 'n> = List.tail v |> Vector

    /// We currently have no way to express an integer literal at the type level, so we use a dummy first argument
    /// which is a vector of the right length.
    let replicate<'a, 'x, 'm, 'n> (Vector len : Vector<'x, 'm>) (Vector v : Vector<'a, 'n>) : Vector<'a, Times<'m, 'n>> =
        List.replicate (List.length len) v
        |> List.collect id
        |> Vector

    [<RequireQualifiedAccess>]
    module Unsafe =
        let cast<'a, 'n, 'm> (Vector a : Vector<'a, 'n>) : Vector<'a, 'm> = Vector a

    /// Allows you to convert safely between Vector<int, 3> and Vector<int, 1 + 2>, say.
    /// You may need to call `cast` multiple times.
    let inline cast< ^n2, ^n1, 'a when (^n1 or ^n2) : (static member refine : ^n1 -> ^n2)> (a : Vector<'a, ^n1>) : Vector<'a, ^n2> = Unsafe.cast a
    let inline cast'< ^n1, ^n2, 'a when (^n1 or ^n2) : (static member refine : ^n1 -> ^n2)> (a : Vector<'a, ^n1>) : Vector<'a, ^n2> = Unsafe.cast a

[<AutoOpen>]
module Patterns =
    let (|Vec|) (Vector v : Vector<'a, S<'n>>) : 'a * Vector<'a, 'n> =
        List.head v, Vector (List.tail v)

    let (|Empty|) (_ : Vector<'a, Z>) = ()

    let ( ~- ) () : Vector<'a, Z> = Vector.empty<'a>
    let ( ** ) (x : 'a) (v : Vector<'a, 'n>) = Vector.cons x v
