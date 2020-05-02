namespace Vector.Test

open Vector
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestVector =
    [<Test>]
    let ``Basic construction`` () =
        let v1 =
            Vector.empty
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6
        v1 |> Vector.toList |> shouldEqual [6 ; 5 ; 3]

        let v2 = "bye" ** "hi" ** -()
        v2 |> Vector.toList |> shouldEqual ["bye" ; "hi"]

    [<Test>]
    let ``Pattern matching`` () =
        match -() with
        | Empty () -> ()
        //| Vec (a, b) -> failwith "" -- doesn't compile

        let v1 =
            6 ** 5 ** 3 ** -()

        match v1 with
        | Vec (a , b) ->
            a |> shouldEqual 6
            match b with
            | Vec (b, c) ->
                b |> shouldEqual 5
                match c with
                | Vec (c, d) ->
                    c |> shouldEqual 3
                    match d with
                    | Empty () -> ()

    [<Test>]
    let ``Vector concatenation`` () =
        let v1 =
            6 ** 5 ** 3 ** -()

        let twice = Vector.append v1 v1
        let otherTwice =
            Vector.empty
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6

        // They are equal vectors...
        twice |> Vector.toList
        |> shouldEqual (otherTwice |> Vector.toList)
        // but their equality doesn't typecheck.
        // twice |> shouldEqual otherTwice

        // But sufficient safe casting can make it typecheck.
        twice
        |> Vector.cast
        |> Vector.cast
        |> Vector.cast
        |> Vector.cast
        |> shouldEqual otherTwice
