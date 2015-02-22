namespace FsReact

module UI =

    open FsReact.Core
    open System.Diagnostics

    type Elements = Elements of Element list
    type Text = Text of string
    type OnClick = OnClick of (Component * obj)
    type Key = Key of string
    type Flex = Flex of int

    let button text event p = native "Button" (Props.concat [Text text; OnClick event] p)
    let text text p = native "Text" (Text text :> obj :: p)
    let view c p = native "View" (Elements c :> obj :: p)

    module VDOM = 

        let private debug = System.Diagnostics.Debug.WriteLine

        type Props = Props.Props

        open Resources
        open Facebook.CSSLayout
        open Reconciler

        type ResourceState = { name: string; resource: Resource; css: CSSNode }

        type MountedState = 
            | ResourceState of ResourceState
            | ComponentState of Component
            with
            member this.unmount() =
                match this with
                | ResourceState { resource = resource; css = css } ->
                    css.RemoveSelf()
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

        type Context = { resources: Resource list }
            with
            member this.mountNested nested =
                match this.resources with
                | [] -> failwithf "failed to mount nested resource %s, no ancestor" (nested.ToString())
                | ancestor :: _ -> ancestor.mountNested nested

            member this.unmountNested nested = 
                match this.resources with
                | [] -> failwithf "failed to unmount nested resource %s, no ancestor" (nested.ToString())
                | ancestor :: _ -> ancestor.unmountNested nested

            member this.push resource = 
                { this with resources = resource :: this.resources }

            static member empty = { resources = [] }

        let derivedKey key (i:int) = key + "." + (i |> string)

        let elementKey key i (element:Element) = 
            match Props.tryGetFromList (function Key key -> key) element.props with
            | Some key -> key
            | None -> derivedKey key i

        let rec mount (context: Context) (key:string) (element : Element) : MountedElement = 
            let props = element.props |> Props.ofList

            match element.kind with
            | Component c ->
                let c = c.createComponent element.props
                let nested = c.render()
                let nestedKey = elementKey key 0 nested
                let nested = mount context nestedKey nested
                let nested = [nested.key, nested] |> Dict.ofList
                { 
                    props = element.props |> Props.ofList; 
                    key = key; 
                    state = ComponentState c; 
                    nested = nested
                }
            | Native name ->
                let resource = Registry.createResource name element.props
                context.mountNested resource
                let nestedContext = context.push resource
                let nested = 
                    props 
                    |> Props.tryGetOr (function Elements nested -> nested) []
                    |> List.mapi (fun i element -> mount nestedContext (elementKey key i element) element)
                    |> List.map (fun m -> m.key, m)
                    |> Dict.ofList
                {
                    props = props;
                    key = key;
                    state = ResourceState { name = name; resource = resource; css = CSSNode() };
                    nested = nested
                }

        let rec unmount (context: Context) (mounted: MountedElement) =
            let nestedContext = 
                match mounted.state with
                | ResourceState r -> context.push r.resource
                | ComponentState _ -> context

            mounted.nested |> Dict.toSeq |> Seq.iter (snd >> unmount nestedContext)

            match mounted.state with
            | ResourceState r -> context.unmountNested r.resource
            | ComponentState _ -> ()

            mounted.state.unmount()

        let rec reconcile (context: Context) (mounted: MountedElement) (element: Element) = 
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
                mount context mounted.key element

        and reconcileNested (context: Context) (mounted: MountedElement) (nested: Element list) =
            let key = mounted.key

            let keyedNested = 
                nested 
                |> List.mapi (fun i element -> elementKey key i element, element)

            let functions = 
                {
                add = 
                    fun k e -> 
                    mount context k e;
                update = 
                    fun k m e -> 
                    assert(k = m.key)
                    reconcile context m e;
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
                reconcile Context.empty root element
            | _ -> failwith "a mounted element at root must be a component"

        let rootKey = ""

 
    type MountedRoot = 
        { mutable root: VDOM.MountedElement }
        member this.update() = this.root <- VDOM.updateRoot this.root

    let mountRoot resource element = 
        let context = VDOM.Context.empty
        let context = context.push resource
        let mounted = VDOM.mount context VDOM.rootKey element
        { root = mounted }

