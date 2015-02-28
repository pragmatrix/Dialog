namespace FsReact

open Resources

type Key = Key of string

type MountedRoot = 
    abstract update : unit -> unit

type ResourceState = { name: string; resource: Resource }
    
module private VDOM = 

    type Props = Props.Props

    open Reconciler

    type MountedState = 
        | ResourceState of ResourceState
        | ComponentState of Component
        with
        member this.unmount() =
            match this with
            | ResourceState { resource = resource } ->
                resource.Dispose()
            | _ -> ()

    let mkIdentity name key = 
        key + "." + name

    type MountedElement = 
        {
            key: string; 
            props: Props;
            state: MountedState; 
            nested: Dict<string, MountedElement>;
        }
        with
        member this.applyProps props =
            let newProps =
                this.props |> Props.apply props
            { this with props = newProps }

        member this.identity =
            match this.state with
            | ResourceState r -> mkIdentity r.name this.key
            | ComponentState _ -> this.key

    type Context = { parent: Context option; element: MountedElement }
        with
        member this.resource =
            this.resources |> Seq.head

        member this.resources = 
            seq {
                yield!
                    match this.element.state with
                    | ResourceState rs -> [rs]
                    | _ -> []

                match this.parent with
                | Some c -> yield! c.resources
                | None -> ()
            }

        member this.canMountNested nested = 
            let nested = nested.resource
            this.resources 
            |> Seq.exists (fun r -> r.resource.canMountNested nested)

        member this.mountNested index nested =
            let nested = nested.resource
            this.resources 
            |> Seq.find (fun r -> r.resource.canMountNested nested)
            |> fun r -> r.resource.mountNested index nested

        member this.unmountNested nested = 
            let nested = nested.resource
            this.resources
            |> Seq.find (fun r -> r.resource.canMountNested nested)
            |> fun r -> r.resource.unmountNested nested

        member this.extend element = 
            { parent = Some this; element = element }
            
        static member root root = { parent= None; element = root }
         
    let derivedKey key (i:int) = key + "." + (i |> string)

    let elementKey key i (element:Element) = 
        match Props.tryGetFromList (function Key key -> key) element.props with
        | Some key -> key
        | None -> derivedKey key i

    let rec mount (context: Context) index (key:string) (element : Element) : MountedElement = 

        let state, nested = 
            match element.kind with
            | Component c ->
                let c = c.createComponent element.props
                Trace.renderingComponent "" key

                ComponentState c, [c.render()]

            | Native name ->
                let identity = mkIdentity name key
                let resource = Registry.createResource name identity element.props
                let state = { name = name; resource = resource }

                if (context.canMountNested state) then
                    Trace.mountingResource context.resource.name index identity
                    context.mountNested index state

                ResourceState state, element.nested

        let mounted = 
            {
                props = element.props |> Props.ofList;
                key = key;
                state = state;
                nested = Dict.ofList []
            }

        let context = context.extend(mounted)

        reconcileNested context mounted nested

    and unmount (context: Context) (mounted: MountedElement) =

        let nestedContext = context.extend mounted
        let mounted = reconcileNested nestedContext mounted []

        match mounted.state with
        | ResourceState r ->
            if (context.canMountNested r) then
                context.unmountNested r
                Trace.unmountedResource context.resource.name mounted.identity
        | ComponentState _ -> ()

        mounted.state.unmount()

    and reconcile (context: Context) index (mounted: MountedElement) (element: Element) = 
        match mounted.state, element.kind with
        | ComponentState c, Component eClass when obj.ReferenceEquals(c._class, eClass) ->
            let mounted = mounted.applyProps element.props
            Trace.renderingComponent "" mounted.key
            let nested = c.render()
            reconcileNested context mounted [nested]

        | ResourceState ln, Native rn  when ln.name = rn ->
            ln.resource.update element.props
            let mounted = mounted.applyProps element.props
            let nested = element.nested
            let nestedContext = context.extend mounted
            reconcileNested nestedContext mounted nested

        | _ ->
            unmount context mounted
            mount context index mounted.key element

    and reconcileNested (context: Context) (mounted: MountedElement) (nested: Element list) =
        let key = mounted.key

        let keyedNested = 
            nested 
            |> List.mapi (fun i element -> elementKey key i element, element)

        let functions = 
            {
            add = 
                fun i k e -> 
                mount context i k e;
            update = 
                fun i k m e -> 
                assert(k = m.key)
                reconcile context i m e;
            remove = 
                fun k m -> 
                assert (k = m.key)
                unmount context m
            }

        let newNested = Reconciler.reconcile functions mounted.nested keyedNested
        { mounted with nested = newNested }

    let updateRoot context root = 
        match root.state with
        | ComponentState c ->
            let element = { Element.kind = Component c._class; props = root.props |> Props.toList; nested = [] }
            reconcile context 0 root element
        | _ -> failwith "a mounted element at root must be a component"

    let rootKey = "/"

    type MountedRoot_ = 
        { 
            context: Context;
            mutable root: MountedElement 
        }
        with
        interface MountedRoot with
            member this.update() = this.root <- updateRoot this.context this.root

    let mountRoot resource element = 
        let root = { key = rootKey; props = Props.ofList []; state = ResourceState resource; nested = Dict.ofList [] }
        let context = Context.root root
        let mounted = mount context 0 rootKey element
        { context = context; root = mounted } :> MountedRoot

