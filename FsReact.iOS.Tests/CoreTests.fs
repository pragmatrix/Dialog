module ComponentTests

open NUnit.Framework

open UIKit
open FsReact
open FsReact.Core

type DummyComponent() =
    interface Component with
        member this.render() = resource "" [] []
        member this.dispatchEvent e = ()
        member this._class = unbox null

[<TestFixture>]
type CoreTests() = 

    [<Test>]
    member this.``function instances can be used as roots``() = 

        let f = fun c e -> ()
        let f2 = fun c e -> ()
        let registered = registerEventRoot f
        let registered2 = registerEventRoot f2
        
        registered()
        registered2()

    [<Test>]
    member this.``event roots can deliver events``() =
        let delivered = ref false
        
        let c = DummyComponent()

        let f = fun c e ->
            delivered := true

        let registered = registerEventRoot f

        let ev = {
            message = null
            props = []
            sender = Reference.empty
        }

        dispatchEvent (c :> Component) ev
        registered()
        Assert.True(!delivered)
    