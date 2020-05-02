namespace Vector

type Z =
    private
    | Z
    static member computed (_ : Z) : unit = ()

type S<'a> =
    private
    | S
    static member inline refine< ^n1, ^n2 when ^n1 : (static member refine : ^n1 -> ^n2)> (_ : S<'n1>) : S<'n2> = Unchecked.defaultof<_>
    static member inline computed< ^n1 when ^n1 : (static member computed : ^n1 -> unit)> (_ : S<'n1>) : unit = ()

type Add<'a, 'b> =
    private
    | Add
    static member inline refine (_ : Add<Z, Z>) : Z = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2> (_ : Add< S< ^n1>, S< ^n2>>) : S<S<Add< ^n1, ^n2>>> = Unchecked.defaultof<_>
    //static member inline refine< ^n1, ^n2 when (^n1 or ^n2) : (static member refine : ^n1 -> ^n2)> (_ : Add< Z, ^n1>) : ^n2 = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2> (_ : Add< Z, ^n1>) : ^n2 = Unchecked.defaultof<_>
    static member inline refine< ^n1, ^n2 > (_ : Add< ^n1, Z>) : ^n2 = Unchecked.defaultof<_>

type Vector<'a, 'len> =
    {
        Elements : 'a list
    }

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
    let empty<'a> : Vector<'a, Z> = { Elements = [] }

    let cons<'a, 'n> (x : 'a) (v : Vector<'a, 'n>) : Vector<'a, S<'n>> =
        {
            Elements = x :: v.Elements
        }

    let toList<'a, 'n> (v : Vector<'a, 'n>) : 'a list = v.Elements

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

    let append<'a, 'n1, 'n2> (v1 : Vector<'a, 'n1>) (v2 : Vector<'a, 'n2>) : Vector<'a, Add<'n1, 'n2>> =
        { Elements = List.append v1.Elements v2.Elements }

    let fold<'a, 's, 'n> (f : 's -> 'a -> 's) (s : 's) (v : Vector<'a, 'n>) =
        List.fold f s v.Elements

    let zip<'a, 'b, 'n> (v1 : Vector<'a, 'n>) (v2 : Vector<'b, 'n>) : Vector<'a * 'b, 'n> =
        {
            Elements = List.zip v1.Elements v2.Elements
        }

    let map<'a, 'b, 'n> (f : 'a -> 'b) (v : Vector<'a, 'n>) : Vector<'b, 'n> =
        {
            Elements = List.map f v.Elements
        }

    let head<'a, 'n> (v : Vector<'a, S<'n>>) : 'a = v.Elements.[0]
    let tail<'a, 'n> (v : Vector<'a, S<'n>>) : Vector<'a, 'n> = { Elements = List.tail v.Elements }
    let unsafeCast<'a, 'n, 'm> (a : Vector<'a, 'n>)  : Vector<'a, 'm> = { Elements = a.Elements }

    let inline cast< ^n2, ^n1, 'a when (^n1 or ^n2) : (static member refine : ^n1 -> ^n2)> (a : Vector<'a, ^n1>) : Vector<'a, ^n2> = unsafeCast a
    let inline cast'< ^n1, ^n2, 'a when (^n1 or ^n2) : (static member refine : ^n1 -> ^n2)> (a : Vector<'a, ^n1>) : Vector<'a, ^n2> = unsafeCast a

[<AutoOpen>]
module Patterns =
    let (|Vec|) (v : Vector<'a, S<'n>>) : 'a * Vector<'a, 'n> =
        List.head v.Elements, { Elements = List.tail v.Elements }

    let (|Empty|) (_ : Vector<'a, Z>) = ()