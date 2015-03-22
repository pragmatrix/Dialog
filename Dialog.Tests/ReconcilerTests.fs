module ReconcilerTests

open NUnit.Framework

open Shared

open Dialog
open Dialog.Core

open Reconciler
open Core.Printf

type Trace = 
    | Insert of int * int * int
    | Update of int * int * int
    | Remove of int * int

[<TestFixture>]
type ReconcilerTests() =

    let interpret (cur: _ list) trace = 
        let l = System.Collections.Generic.List(cur)
        
        let applyTrace t = 
            match t with
            | Insert (i, k, v) -> l.Insert(i, (k, v))
            | Update (i, k, v) ->
                Assert.That(i, Is.LessThan(l.Count))
                l.[i] <- (k, v)
            | Remove (k, v) -> l.Remove (k, v) |> ignore

        trace |> List.iter applyTrace

        l |> List.ofSeq

    let reconcile next curl = 
        let cur = Dict.ofList curl

        let log : Trace list ref = ref []

        let trace entry = 
            log := entry :: !log

        let functions = 
            {
                insert = fun i k v -> Insert (i, k, v) |> trace; v
                update = fun i k g v -> Update (i, k, v) |> trace; v
                remove = fun k g -> Remove (k, g) |> trace
            }

        let r = Reconciler.reconcile functions cur next
        let rList = r |> Dict.toSeq |> Seq.toList
        Assert.AreEqual (next, rList)
        let interpretedNext = !log |> List.rev |> interpret curl
        Assert.AreEqual (next, interpretedNext)
        !log |> List.rev


    let equals (expected : Trace list) (result : Trace list) = 
        Assert.AreEqual (expected, result)

    [<Test>]
    member this.``simple update``() = 
        [0, 10; 1, 11]
        |> reconcile [0, 12; 1, 13]
        |> equals [Update (0, 0, 12); Update (1, 1, 13)]

    [<Test>]
    member this.``remove head``() = 
        [0, 10; 1, 11]
        |> reconcile [1, 11]
        |> equals [Remove (0, 10); Update (0, 1, 11)]

    [<Test>]
    member this.``remove interleaved``() = 
        [0, 10; 1, 11; 2, 12; 3, 13]
        |> reconcile [1, 11; 3, 13]
        |> equals [Remove (0,10); Remove (2,12); Update (0,1,11); Update (1,3,13)]

    [<Test>]
    member this.``insert``() = 
        [0, 10]
        |> reconcile [1, 11; 0, 10]
        |> equals [Insert (0,1,11); Update (1,0,10)]

    [<Test>]
    member this.``insert interleaved``() = 
        [0, 10; 1, 11]
        |> reconcile [3, 13; 0, 10; 2, 12; 1, 11]
        |> equals [Insert (0,3,13); Update (1,0,10); Insert (2,2,12); Update (3,1,11)]

    [<Test>]
    member this.``remove all``() = 
        [0, 10; 1, 11]
        |> reconcile []
        |> equals [Remove (0,10); Remove (1,11)]

    [<Test>]
    member this.``setup all``() = 
        []
        |> reconcile [0, 10; 1, 11]
        |> equals [Insert (0,0,10); Insert (1,1,11)]


