namespace FsReact

module UI =

    open FsReact.Core
    open System.Diagnostics

    type Elements = Elements of Element list
    type Text = Text of string
    type OnClick = OnClick of (Component * obj)
    type Key = Key of string

    type Flex = Flex of int

    type Align = 
        | Auto
        | Start
        | Center
        | End
        | Stretch

    type AlignItems = AlignItems of Align
        with
        static member Auto = AlignItems Auto
        static member Start = AlignItems Start
        static member Center = AlignItems Center
        static member End = AlignItems End
        static member Stretch = AlignItems Stretch

    type Justify =
        | Start
        | Center
        | End
        | SpaceBetween
        | SpaceAround

    type JustifyContent = JustifyContent of Justify
        with
        static member Start = JustifyContent Start
        static member Center = JustifyContent Center
        static member End = JustifyContent End
        static member SpaceBetween = JustifyContent SpaceBetween
        static member SpaceAround = JustifyContent SpaceAround


    let button text event p = native "Button" (Props.concat [Text text; OnClick event] p)
    let text text p = native "Text" (Text text :> obj :: p)
    let view c p = native "View" (Elements c :> obj :: p)

    module VDOM = 

        let private debug = System.Diagnostics.Debug.WriteLine

        type Props = Props.Props

        open Resources
        open Reconciler

        type ResourceState = { name: string; resource: Resource }

        type MountedState = 
            | ResourceState of ResourceState
            | ComponentState of Component
            with
            member this.unmount() =
                match this with
                | ResourceState { resource = resource } ->
                    resource.Dispose()
                | _ -> ()

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
                let resource = Registry.createResource name element.props
                let state = { name = name; resource = resource }
                context.mountNested index state
                let nestedContext = context.push state
                let nested = 
                    props 
                    |> Props.tryGetOr (function Elements nested -> nested) []
                    |> List.mapi (fun i element -> mount nestedContext i (elementKey key i element) element)
                    |> List.map (fun m -> m.key, m)
                    |> Dict.ofList
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
            | ResourceState r -> context.unmountNested r
            | ComponentState _ -> ()

            mounted.state.unmount()

        let rec reconcile (context: Context) index (mounted: MountedElement) (element: Element) = 
            match mounted.state, element.kind with
            | ComponentState c, Component eClass when obj.ReferenceEquals(c._class, eClass) ->
                let mounted = mounted.applyProps element.props
                let nested = c.render()
                reconcileNested context mounted [nested]

            | ResourceState ln, Native rn  when ln.name = rn ->
                ln.resource.update element.props
                let mounted = mounted.applyProps element.props

                let nested = 
                    element.props
                    |> Props.ofList
                    |> Props.tryGetOr (function Elements nested -> nested) []
                   
                reconcileNested context mounted nested

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

        let updateRoot root = 
            match root.state with
            | ComponentState c ->
                let element = { Element.kind = Component c._class; props = root.props |> Props.toList }
                reconcile Context.empty 0 root element
            | _ -> failwith "a mounted element at root must be a component"

        let rootKey = ""

 
    type MountedRoot = 
        { mutable root: VDOM.MountedElement }
        member this.update() = this.root <- VDOM.updateRoot this.root

    let mountRoot resource element = 
        let context = VDOM.Context.empty
        let context = context.push resource
        let mounted = VDOM.mount context 0 VDOM.rootKey element
        { root = mounted }

