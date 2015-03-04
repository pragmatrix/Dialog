module ComponentTests

open NUnit.Framework

open FsReact
open FsReact.Core

type DummyComponent() =
    interface Component with
        member this.render() = service "" [] []
        member this.dispatchEvent e = ()
        member this.class' = unbox null
        member this.properties = []

[<TestFixture>]
type CoreTests() = 

    [<Test>]
    member this.``function instances can be used as roots``() = 

        let f = fun c e -> ()
        let f2 = fun c e -> ()
        let registered = Events.registerEventRoot f
        let registered2 = Events.registerEventRoot f2
        
        registered()
        registered2()

    [<Test>]
    member this.``event roots can deliver events``() =
        let delivered = ref false
        
        let c = DummyComponent()

        let f = fun c e ->
            delivered := true

        let registered = Events.registerEventRoot f

        let ev = {
            message = null
            props = []
            sender = Reference.empty
        }

        Events.dispatchEvent (c :> Component) ev
        registered()
        Assert.True(!delivered)
    