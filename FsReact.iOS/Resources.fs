namespace FsReact

module Resources =

    open FsReact
    open PropertyWriter
    open System

    type Resource =
        inherit IDisposable
        abstract update : Properties -> unit
        abstract instance : obj

    type Resource<'resource>(instance: 'resource, writer: PropertyWriter<'resource>, disposer: 'resource -> unit) =
        let reconciler = PropertyReconciler<'resource>(writer, instance)
        interface Resource with
            member this.instance = instance :> obj
            member this.update props = reconciler.update props
            member this.Dispose() = disposer instance

    let createResource writer disposer instance initialProps = 
        let r = new Resource<_>(instance, writer, disposer)
        (r :> Resource).update initialProps
        r

    let mutable private registry = Map.empty

    module Registry =

        let register (name:string) (f: Properties -> Resource<'resource>) = 
            let f p = f p :> Resource
            registry <- registry.Add(name, f)

        let createResource name props =
            registry.[name] props