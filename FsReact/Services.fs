namespace FsReact

module Services =

    open System
    open FsReact
    open ComponentDOM
    open Scanners

    type MountingNotifications = 
        // notified after all nested elements were mounted
        abstract mounted : unit -> unit
        // notified before all nested elements will unmount
        abstract unmounting : unit -> unit

    type Service =
        inherit IDisposable
        abstract identity: Identity
        abstract update : Properties -> unit
        abstract instance : obj
        abstract updateNested : MountedElement -> unit

        abstract mounted : unit -> unit
        abstract unmounting : unit -> unit

    (* Registration of Services *)

    let mutable private registry = Map.empty<string, string * (Identity -> Properties -> Service)>
    
    let instantiateService ((name, key) as identity) props =
        let f = registry.[name] |> snd 
        f identity props

    let typeOf name = registry.[name] |> fst

    let recursiveNativeTypeScanner typeTest = 
        Scanners.recursiveNativeNameScanner (typeOf >> typeTest)

    type NestingAdapter<'target> = 
        { mount : 'target -> int -> obj -> unit; unmount : 'target -> obj -> unit }
        static member agnostic<'t>() : NestingAdapter<'t> = { mount = (fun _ _ _ -> ()); unmount = (fun _ _ -> ()) }

    (* Service specification *)

    type ServiceClass<'instance> = { 
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
        member this.NestingAdapter (mounter : 'instance -> int -> 'n -> unit, unmounter : 'instance-> 'n -> unit) =
            let mounter t i n = mounter t i (unbox n)
            let unmounter t n = unmounter t (unbox n)
            { this with nestingAdapter = { mount = mounter; unmount = unmounter }}
        member this.Scanner scanner = { this with scanner = scanner }
        member this.PropertyWriter writer = { this with propertyWriter = writer }
        member this.UpdateNotifier notifier = { this with updateNotifier = notifier }
        member this.MountingNotifier notifier = { this with mountingNotifier = notifier }

    type Define with

        static member Service() = 

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
                scanner = Scanners.dontScan;
                updateNotifier = fun _ -> ();
                mountingNotifier = notifyMounted, notifyUnmounting
            }
 
    let mkNestedServiceKey i (mounted : MountedElement) = 
        match Props.tryGet (fun (Key k) -> k) mounted.props with
        | Some k -> k
        | None -> i.ToString()

    type Service<'service>
        (
            class': ServiceClass<'service>,
            identity: Identity
        ) =

        let _class = class'
        let _instance = class'.constructor'()
        let _writer = class'.propertyWriter
        let _nestingAdapter = class'.nestingAdapter
        let _disposer = class'.destructor

        let mutable _nested = Dict<string, Service>()
        
        let mkIdentityString identity = fst identity + ":" + snd identity
        let _identityString = fst identity + ":" + snd identity

        let _propertyReconciler = PropertyReconciler<'service>(_writer, _instance, _identityString)

        member this.instance = _instance

        member this.mountNested index mounted = 
            match mounted.state with
            | NativeState name ->
                let key = mkNestedServiceKey index mounted
                let identity = ComponentDOM.mkIdentity name key
                Trace.mountingService _identityString index (mkIdentityString identity)
                let service = instantiateService identity (mounted.props |> Props.toProperties)
                service.updateNested mounted
                _nestingAdapter.mount _instance index service.instance
                service.mounted()
                service

            | _ -> failwith "internal error: can't create services for non-native elements, this might be caused by a defective scanner"

        member this.unmountNested (nested:Service) = 
            nested.unmounting()
            _nestingAdapter.unmount _instance nested.instance
            Trace.unmountingService _identityString (mkIdentityString nested.identity)
            nested.Dispose()

        member this.updateNested index (nested : Service) mounted = 
            match mounted.state with
            | NativeState name ->
                if fst nested.identity = name then
                    nested.update (mounted.props |> Props.toProperties)
                    nested.updateNested mounted
                    nested
                else
                    this.unmountNested nested
                    this.mountNested index mounted

            | _ -> failwith "internal error: a services can not be updated with a component, this may be caused by scanner that returns components"

        member this.reconcileNested keyedMounts =
            let functions : Reconciler.Functions<_,_,_> = 
                {
                    insert = fun i k v -> this.mountNested i v;
                    update = fun i k service v -> this.updateNested i service v
                    remove = fun k service -> this.unmountNested service
                }

            _nested <- Reconciler.reconcile functions _nested keyedMounts
   
        interface Service with
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
                    |> List.mapi (fun i m -> mkNestedServiceKey i m, m)
                let this = this :> Service<_>
                this.reconcileNested keyedMounts
                class'.updateNotifier _instance

    type ServiceClass<'instance> with
        member this.Instantiate identity initialProps = 
            let r = 
                new Service<_>(this, identity)
            (r:>Service).update initialProps
            r

    type ServiceReference<'target>(target: 'target, reader: PropertyReader<'target>) = 
        interface Reference with
            override this.get deconstructor = reader.read target deconstructor


    module Registry =

        let register (name:string) (type': string) (c: ServiceClass<_>) = 
            let f = c.Instantiate
            let f i p = f i p :> Service
            registry <- registry.Add(name, (type', f))


    let updateElementRoot root = 
        match root.state with
        | ComponentState c ->
            let element = { Element.kind = Component c.class'; properties = root.props |> Props.toProperties; nested = [] }
            reconcile root element

        | _ -> failwith "a mounted element at root must be a component"

    let rootKey = "/"

    type MountedRoot_ = 
        { 
            services: Service list;
            mutable root: MountedElement 
        }
        with
        interface MountedRoot with
            member this.update() = 
                this.root <- updateElementRoot this.root
                this.services 
                |> List.iter (fun r -> r.updateNested this.root)

    let mountRoot (services: Service list) element = 
        let mounted = mount rootKey element
        services
        |> List.iter (fun r -> r.updateNested mounted)
        { services = services; root = mounted } :> MountedRoot

    type SystemService() =
        class end


    let private systemServicePrototype = 
        Define.Service()
            .Constructor(fun () -> SystemService())

    let createSystemService (nestedRootTypes : string list) = 

        let inclusionTest t =
            let include' = 
                nestedRootTypes
                |> List.exists ((=) t)
            if include' then Include else ScanNested

        let scanner = recursiveNativeTypeScanner inclusionTest

        systemServicePrototype
            .Scanner(scanner)
            .Instantiate
  