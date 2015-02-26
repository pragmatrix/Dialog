namespace FsReact

open Resources

type Key = Key of string

type MountedRoot = 
    abstract update : unit -> unit

type ResourceState = { name: string; resource: Resource }
    
module private VDOM = 

    let private debug = System.Diagnostics.Debug.WriteLine

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

    type Context = { states: ResourceState list }
        with
        member this.mountNested index nested =
            match this.states with
            | [] -> failwithf "failed to mount nested resource %s, no ancestor" (nested.ToString())
            | ancestor :: _ -> 
            ancestor.resource.mountNested index nested.resource

        member this.unmountNested nested = 
            match this.states with
            | [] -> failwithf "failed to unmount nested resource %s, no ancestor" (nested.ToString())
            | ancestor :: _ -> 
            ancestor.resource.unmountNested nested.resource

        member this.push state = 
            { this with states = state :: this.states }

        member this.head = 
            this.states |> List.head

        static member empty = { states = [] }

    let derivedKey key (i:int) = key + "." + (i |> string)

    let elementKey key i (element:Element) = 
        match Props.tryGetFromList (function Key key -> key) element.props with
        | Some key -> key
        | None -> derivedKey key i

    let rec mount (context: Context) index (key:string) (element : Element) : MountedElement = 
        let props = element.props |> Props.ofList

        match element.kind with
        | Component c ->
            let c = c.createComponent element.props
            Trace.renderingComponent "" key
            let nested = c.render()
            let nestedKey = elementKey key 0 nested
            let nested = mount context 0 nestedKey nested
            let nested = [nested.key, nested] |> Dict.ofList
            { 
                props = element.props |> Props.ofList; 
                key = key; 
                state = ComponentState c; 
                nested = nested
            }
        | Native name ->
            let identity = mkIdentity name key
            let resource = Registry.createResource name identity element.props
            let state = { name = name; resource = resource }

            Trace.mountingResource context.head.name index identity
            context.mountNested index state

            let nestedContext = context.push state
            let nested = 
                element.nested
                |> Seq.mapi (fun i element -> mount nestedContext i (elementKey key i element) element)
                |> Seq.map (fun m -> m.key, m)
                |> Dict.ofSeq
            {
                props = props;
                key = key;
                state = ResourceState state;
                nested = nested
            }

    let rec unmount (context: Context) (mounted: MountedElement) =
        let nestedContext = 
            match mounted.state with
            | ResourceState r -> context.push r
            | ComponentState _ -> context

        mounted.nested |> Dict.toSeq |> Seq.iter (snd >> unmount nestedContext)

        match mounted.state with
        | ResourceState r ->
            context.unmountNested r
            Trace.unmountedResource context.head.name mounted.identity
        | ComponentState _ -> ()

        mounted.state.unmount()

    let rec reconcile (context: Context) index (mounted: MountedElement) (element: Element) = 
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
            let nestedContext = context.push ln
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
        let context = Context.empty
        let context = context.push resource
        let mounted = mount context 0 rootKey element
        { context = context; root = mounted } :> MountedRoot

