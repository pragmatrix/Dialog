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
        class end
        with 
        interface Resource
            with
            member this.instance = instance :> obj
            member this.update props = writer.write instance props
            member this.Dispose() = disposer instance

    let createResource instance writer disposer = new Resource<_>(instance, writer, disposer)

    let mutable private registry = Map.empty

    module Registry =

        let register (name:string) (f: Properties -> Resource<'resource>) = 
            let f p = f p :> Resource
            registry <- registry.Add(name, f)

        let createResource name props =
            registry.[name] props