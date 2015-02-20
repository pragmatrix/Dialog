module ComponentTests

open NUnit.Framework

open UIKit
open FsReact
open FsReact.Core

type DummyComponent() =
    interface Component with
        member this.render() = native "" []
        member this.dispatchEvent e = ()
        member this._class = unbox null

[<TestFixture>]
type CoreTests() = 

    [<Test>]
    member this.``function instances can be used as roots``() = 

        let f = fun e -> ()
        let f2 = fun e -> ()
        let registered = registerEventRoot f
        let registered2 = registerEventRoot f2
        
        registered()
        registered2()

    [<Test>]
    member this.``event roots can deliver events``() =
        let delivered = ref false
        
        let c = DummyComponent()

        let f = fun e ->
            delivered := true

        let registered = registerEventRoot f
        dispatchEvent (c :> Component, null)
        registered()
        Assert.True(!delivered)



    