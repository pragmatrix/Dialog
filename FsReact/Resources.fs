namespace FsReact

module Resources =

    open FsReact
    open System
    open ComponentDOM

    type MountingNotifications = 
        // notified after all nested elements were mounted
        abstract mounted : unit -> unit
        // notified before all nested elements will unmount
        abstract unmounting : unit -> unit

    type Resource =
        inherit IDisposable
        abstract identity: Identity
        abstract update : Properties -> unit
        abstract instance : obj
        abstract updateNested : MountedElement -> unit

        abstract mounted : unit -> unit
        abstract unmounting : unit -> unit


    (* Registration of resource constructors *)

    let mutable private registry = Map.empty<string, string * (Identity -> Properties -> Resource)>
    
    let instantiateResource ((name, key) as identity) props =
        let f = registry.[name] |> snd 
        f identity props

    let typeOf name = registry.[name] |> fst

    let recursiveNativeTypeScanner typeTest = 
        ScanningStrategies.recursiveNativeNameScanner (typeOf >> typeTest)

    (* Resource specification *)

    type ResourceClass<'instance> = { 
            constructor': unit -> 'instance; 
            destructor: 'instance -> unit; 
            propertyWriter: PropertyWriter<'instance>;
            nestingAdapter: NestingAdapter<'instance>;
            scanner: Scanner;
            updateNotifier: 'instance -> unit;
            mountingNotifier: ('instance -> unit) * ('instance -> unit);
        }
        with 
        member this.Constructor c = { this with constructor' = c }
        member this.Destructor d = { this with destructor = d }
        member this.Writer w = { this with propertyWriter = w }
        member this.NestingAdapter na = { this with nestingAdapter = na }
        member this.NestingAdapter (mounter, unmounter) = { this with nestingAdapter = NestingAdapter<_, _>(mounter, unmounter)}
        member this.Scanner scanner = { this with scanner = scanner }
        member this.PropertyWriter writer = { this with propertyWriter = writer }
        member this.UpdateNotifier notifier = { this with updateNotifier = notifier }
        member this.MountingNotifier notifier = { this with mountingNotifier = notifier }

    type Define with
        static member ResourceClass() = 

            let notifyMounted i = 
                match box i with
                | :? MountingNotifications as mn -> mn.mounted()
                | _ -> ()

            let notifyUnmounting i = 
                match box i with
                | :? MountingNotifications as mn -> mn.unmounting()
                | _ -> ()

            { 
                constructor' = fun () -> failwith "not implemented"; 
                destructor = fun _ -> (); 
                propertyWriter = PropertyAccessor.writerFor; 
                nestingAdapter = NestingAdapter<_>.agnostic();
                scanner = ScanningStrategies.dontScan;
                updateNotifier = fun _ -> ();
                mountingNotifier = notifyMounted, notifyUnmounting
            }
 
    let mkNestedResourceKey i (mounted : MountedElement) = 
        match Props.tryGet (fun (Key k) -> k) mounted.props with
        | Some k -> k
        | None -> i.ToString()

    type Resource<'resource>
        (
            class': ResourceClass<'resource>,
            identity: Identity
        ) =

        let _class = class'
        let _instance = class'.constructor'()
        let _writer = class'.propertyWriter
        let _nestingAdapter = class'.nestingAdapter
        let _disposer = class'.destructor

        let mutable _nested = Dict<string, Resource>()
        
        let mkIdentityString identity = fst identity + ":" + snd identity
        let _identityString = fst identity + ":" + snd identity

        let _propertyReconciler = PropertyReconciler<'resource>(_writer, _instance, _identityString)

        member this.instance = _instance

        member this.mountNested index mounted = 
            match mounted.state with
            | NativeState name ->
                let resourceKey = mkNestedResourceKey index mounted
                let identity = ComponentDOM.mkIdentity name resourceKey
                Trace.mountingResource _identityString index (mkIdentityString identity)
                let resource = instantiateResource identity (mounted.props |> Props.toList)
                resource.updateNested mounted
                _nestingAdapter.mount _instance index resource.instance
                resource.mounted()
                resource

            | _ -> failwith "internal error: can't create resources for non-native elements"

        member this.unmountNested (resource:Resource) = 
            resource.unmounting()
            _nestingAdapter.unmount _instance resource.instance
            Trace.unmountedResource _identityString (mkIdentityString resource.identity)
            resource.Dispose()

        member this.updateNested index (resource : Resource) mounted = 
            match mounted.state with
            | NativeState name ->
                if fst resource.identity = name then
                    resource.update (mounted.props |> Props.toList)
                    resource.updateNested mounted
                    resource
                else
                    this.unmountNested resource
                    this.mountNested index mounted

            | _ -> failwith "internal error: a resource can not be updated with a component"

        member this.reconcileNested keyedMounts =
            let functions : Reconciler.Functions<_,_,_> = 
                {
                    insert = fun i k v -> this.mountNested i v;
                    update = fun i k resource v -> this.updateNested i resource v
                    remove = fun k resource -> this.unmountNested resource
                }

            _nested <- Reconciler.reconcile functions _nested keyedMounts
   
        interface Resource with
            member __.identity = identity
            member __.instance = _instance :> obj
            member __.update props = _propertyReconciler.update props
            member __.Dispose() = _disposer _instance

            member this.mounted() = 
                (fst class'.mountingNotifier) _instance
            
            member this.unmounting() =
                (snd class'.mountingNotifier) _instance
                this.reconcileNested []
           
            member this.updateNested mounted =
                let mounts = class'.scanner mounted
                let keyedMounts = 
                    mounts 
                    |> List.mapi (fun i m -> mkNestedResourceKey i m, m)
                let this = this :> Resource<_>
                this.reconcileNested keyedMounts
                class'.updateNotifier _instance

    type ResourceClass<'instance> with
        member this.Instantiate identity initialProps = 
            let r = 
                new Resource<_>(this, identity)
            (r:>Resource).update initialProps
            r

    (* Resource references *)

    type ResourceReference<'target>(target: 'target, reader: PropertyReader<'target>) = 
        interface Reference with
            override this.get deconstructor = reader.read target deconstructor


    module Registry =

        let register (name:string) (type': string) (c: ResourceClass<_>) = 
            let f = c.Instantiate
            let f i p = f i p :> Resource
            registry <- registry.Add(name, (type', f))


    let updateElementRoot root = 
        match root.state with
        | ComponentState c ->
            let element = { Element.kind = Component c._class; props = root.props |> Props.toList; nested = [] }
            reconcile root element

        | _ -> failwith "a mounted element at root must be a component"

    let rootKey = "/"

    type MountedRoot_ = 
        { 
            resources: Resource list;
            mutable root: MountedElement 
        }
        with
        interface MountedRoot with
            member this.update() = 
                this.root <- updateElementRoot this.root
                this.resources 
                |> List.iter (fun r -> r.updateNested this.root)

    let mountRoot (resources: Resource list) element = 
        let mounted = mount rootKey element
        resources
        |> List.iter (fun r -> r.updateNested mounted)
        { resources = resources; root = mounted } :> MountedRoot

    type SystemResource() =
        class end


    let private systemResourceClassPrototype = 
        Define.ResourceClass()
            .Constructor(fun () -> SystemResource())

    open ScanningStrategies

    let createSystemResource (nestedRootTypes : string list) = 

        let inclusionTest t =
            let include' = 
                nestedRootTypes
                |> List.exists ((=) t)
            if include' then Include else ScanNested

        let scanner = recursiveNativeTypeScanner inclusionTest

        systemResourceClassPrototype
            .Scanner(scanner)
            .Instantiate
  