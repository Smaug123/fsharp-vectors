namespace Vector.Test

open Vector
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestVector =
    [<Test>]
    let example1 () =
        let v1 =
            Vector.empty
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6
        v1 |> Vector.toList |> shouldEqual [6 ; 5 ; 3]

        let v2 = Vector.empty |> Vector.cons "hi" |> Vector.cons "bye"
        v2 |> Vector.toList |> shouldEqual ["bye" ; "hi"]

        match Vector.empty with
        | Empty () -> failwith ""
        //| Vec (a, b) -> failwith "" -- doesn't compile

        match v1 with
        | Vec (a , b) ->
            a |> shouldEqual 6
            b |> Vector.toList |> shouldEqual [5 ; 3]

        let v1 =
            Vector.empty
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6

        let twice = Vector.append v1 v1
        let otherTwice =
            Vector.empty
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6
            |> Vector.cons 3
            |> Vector.cons 5
            |> Vector.cons 6

        twice |> Vector.toList
        |> shouldEqual (otherTwice |> Vector.toList)

        twice
        |> Vector.cast
        |> Vector.cast<S<S<_>>, _, _>
        |> Vector.cast<S<S<S<S<_>>>>, _, _>
        |> Vector.cast
        |> shouldEqual otherTwice

    [<Test>]
    let foo () =
        let v1 = Vector.empty |> Vector.cons 1
        let twice = Vector.append v1 v1
        let twiceAgain = Vector.empty |> Vector.cons 1 |> Vector.cons 1

        twice
        |> Vector.cast
        |> Vector.cast
        |> shouldEqual twiceAgain

