namespace FsReact

module Resources =

    open FsReact
    open System

    type Resource =
        inherit IDisposable
        abstract update : Properties -> unit
        abstract instance : obj
        abstract mountNested : int -> Resource -> unit
        abstract unmountNested : Resource -> unit

    type Resource<'resource>
        (
            identity: string,
            instance: 'resource,
            writer: PropertyAccessor<'resource>, 
            nestingAdapter: NestingAdapter<'resource, obj>,
            disposer: 'resource -> unit
        ) =

        let reconciler = PropertyReconciler<'resource>(writer, instance, identity)
        interface Resource with
            member __.instance = instance :> obj
            member __.update props = reconciler.update props
            member __.Dispose() = disposer instance
            member __.mountNested index nested = nestingAdapter.mount instance index nested.instance
            member __.unmountNested nested = nestingAdapter.unmount instance nested.instance

    let createResource writer disposer identity instance initialProps = 
        let r = new Resource<_>(identity, instance, writer, NestingAdapter.invalid(), disposer)
        (r :> Resource).update initialProps
        r

    let createAncestorResource writer disposer (nestingAdapter: NestingAdapter<'target, 'nested>) identity instance initialProps = 
        let r = new Resource<_>(identity, instance, writer, nestingAdapter.promoteNested(), disposer)
        (r :> Resource).update initialProps
        r

    (* Resource references *)

    type ResourceReference<'target>(target: 'target, reader: PropertyReader<'target>) = 
        interface Reference with
            override this.get deconstructor = reader.read target deconstructor

    (* Registration of resource constructors *)

    let mutable private registry = Map.empty

    module Registry =

        let register (name:string) (f: string -> Properties -> Resource<'resource>) = 
            let f i p = f i p :> Resource
            registry <- registry.Add(name, f)

        let createResource name identity props =
            registry.[name] identity props