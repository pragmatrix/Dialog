﻿namespace Dialog

module Services =

    open System
    open Dialog
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

    let typeOf serviceRef = serviceRef.serviceType

    let recursiveNativeTypeScanner typeTest = 
        Scanners.recursiveNativeNameScanner (typeOf >> typeTest)

    type NestingAdapter<'target> = 
        { mount : 'target -> int -> obj -> unit; unmount : 'target -> obj -> unit }
        static member agnostic<'t>() : NestingAdapter<'t> = { mount = (fun _ _ _ -> ()); unmount = (fun _ _ -> ()) }

    (* Service specification *)

    type ServiceClass<'instance> = { 
            constructor': unit -> 'instance; 
            destructor: 'instance -> unit; 
            propertyAccessor: PropertyAccessor<'instance>;
            nestingAdapter: NestingAdapter<'instance>;
            scanner: Scanner;
            updateNotifier: 'instance -> unit;
            mountingNotifier: ('instance -> unit) * ('instance -> unit);
        }
        with 
        member this.Constructor c = { this with constructor' = c }
        member this.Destructor d = { this with destructor = d }
        member this.Writer w = { this with propertyAccessor = w }
        member this.NestingAdapter (mounter : 'instance -> int -> 'n -> unit, unmounter : 'instance-> 'n -> unit) =
            let mounter t i n = mounter t i (unbox n)
            let unmounter t n = unmounter t (unbox n)
            { this with nestingAdapter = { mount = mounter; unmount = unmounter }}
        member this.Scanner scanner = { this with scanner = scanner }
        member this.PropertyAccessor accessor = { this with propertyAccessor = accessor }
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
                propertyAccessor = PropertyAccessor.accessorFor |> PropertyAccessor.materialize; 
                nestingAdapter = NestingAdapter<_>.agnostic();
                scanner = Scanners.dontScan;
                updateNotifier = fun _ -> ();
                mountingNotifier = notifyMounted, notifyUnmounting
            }
 
    let mkNestedServiceKey i (mounted : MountedElement) = 
        match mounted.state with
        | ServiceState (_, properties) ->
            match Properties.tryGet (fun (Key k) -> k) properties with
            | Some k -> k
            | None -> i.ToString()
        | _ -> failwith "nested mounted elements must be a service to mount it to a parent service, please review your scanner"

    type Service<'service>
        (
            class': ServiceClass<'service>,
            identity: Identity
        ) as this =

        let _class = class'
        let _instance = class'.constructor'()
        let _writer = class'.propertyAccessor
        let _nestingAdapter = class'.nestingAdapter
        let _disposer = class'.destructor

        let mutable _nested = Dict<string, Service>()
        
        let mkIdentityString identity = fst identity + ":" + snd identity
        let _identityString = fst identity + ":" + snd identity

        let _propertyReconciler = PropertyReconciler<'service>(_writer, _instance, _identityString)

        let _reconcilerFunctions : Reconciler.Functions<_,_,_> = 
            {
                insert = fun i _ -> this.mountNested i
                update = fun i _ -> this.updateNested i
                remove = fun _ -> this.unmountNested
            }

        member this.instance = _instance

        member this.mountNested index mounted = 
            match mounted.state with
            | ServiceState (serviceRef, properties) ->
                let key = mkNestedServiceKey index mounted
                let identity = ComponentDOM.mkIdentity serviceRef.name key
                Trace.mountingService _identityString index (mkIdentityString identity)
                let service : Service = serviceRef.instantiate.Value identity properties |> unbox
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
            | ServiceState (serviceRef, properties) ->
                if fst nested.identity = serviceRef.name then
                    nested.update properties
                    nested.updateNested mounted
                    nested
                else
                    this.unmountNested nested
                    this.mountNested index mounted

            | _ -> failwith "internal error: a service can not be updated with a component, this may be caused by scanner that returns components"

        member this.reconcileNested keyedMounts =
            _nested <- Reconciler.reconcile _reconcilerFunctions _nested keyedMounts
   
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

    let rootKey = "/"

    type MountedRoot_ = 
        { 
            services: Service list;
            rootElement: Element;
            mutable root: MountedElement 
        }
        with
        interface MountedRoot with
            member this.update() = 
                this.root <- reconcile this.root this.rootElement
                this.services 
                |> List.iter (fun r -> r.updateNested this.root)

    let mountRoot (services: Service list) element = 
        let mounted = mount rootKey element
        services
        |> List.iter (fun r -> r.updateNested mounted)
        { services = services; rootElement = element; root = mounted } :> MountedRoot

    type SystemService() =
        class end


    let private systemServicePrototype = 
        Define.Service()
            .Constructor(fun () -> SystemService())

    let createSystemService (nestedRootTypes : ServiceType list) = 

        let inclusionTest t =
            let include' = 
                nestedRootTypes
                |> List.exists ((=) t)
            if include' then Include else ScanNested

        let scanner = recursiveNativeTypeScanner inclusionTest

        systemServicePrototype
            .Scanner(scanner)
            .Instantiate
  