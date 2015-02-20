namespace FsReact

module UI =

    open FsReact.Core

    type Elements = Elements of Element list
    type Text = Text of string
    type OnClick = OnClick of (Component * obj)
    type Key = Key of string
    type Flex = Flex of int

    let button text event p = native "Button" (Props.concat [Text text; OnClick event] p)
    let text text p = native "Text" (Text text :> obj :: p)
    let view c p = native "View" (Elements c :> obj :: p)

    module VDOM = 

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

        type Context = { key: string; index: int }

        let mkContext key i = { key = key; index = i }

        let derivedKey key (i:int) = key + "." + (i |> string)

        let elementKey key i (element:Element) = 
            match Props.tryGetFromList (function Key key -> key) element.props with
            | Some key -> key
            | None -> derivedKey key i

        let rec mount (key:string) (element : Element) : MountedElement = 
            let props = element.props |> Props.ofList

            match element.kind with
            | Component c ->
                let c = c.createComponent element.props
                let nested = c.render()
                let nestedKey = elementKey key 0 nested
                let nested = mount nestedKey nested
                let nested = [nested.key, nested] |> Dict.ofList
                { 
                    props = element.props |> Props.ofList; 
                    key = key; 
                    state = ComponentState c; 
                    nested = nested
                }

            | Native name ->
                let resource = Registry.createResource name element.props
                let nested = 
                    props 
                    |> Props.tryGetOr (function Elements nested -> nested) []
                    |> List.mapi (fun i element -> mount (elementKey key i element) element)
                    |> List.map (fun m -> m.key, m)
                    |> Dict.ofList

                {
                    props = props;
                    key = key;
                    state = ResourceState { name = name; resource = resource; css = CSSNode() };
                    nested = nested
                }

        let rec unmount (mounted: MountedElement) =
            mounted.nested |> Dict.toSeq |> Seq.iter (snd >> unmount)
            mounted.state.unmount()

        let rec reconcile (mounted: MountedElement) (element: Element) = 
            match mounted.state, element.kind with
            | ComponentState c, Component eClass when obj.ReferenceEquals(c._class, eClass) ->
                let mounted = mounted.applyProps element.props
                let nested = c.render()
                reconcileNested mounted [nested]

            | ResourceState ln, Native rn  when ln.name = rn ->
                ln.resource.update element.props
                mounted.applyProps element.props
            | _ ->
                unmount mounted
                mount mounted.key element

        and reconcileNested (mounted: MountedElement) (nested: Element list) =
            let key = mounted.key

            let keyedNested = 
                nested 
                |> List.mapi (fun i element -> elementKey key i element, element)

            let functions = 
                {
                    add = (fun k e -> mount k e);
                    update = fun k m e -> 
                        assert(k = m.key)
                        reconcile m e;
                    remove = 
                        fun k m -> 
                        assert (k = m.key)
                        unmount m
                }

            let newNested = Reconciler.reconcile functions mounted.nested keyedNested
            { mounted with nested = newNested }

        let updateRoot root = 
            match root.state with
            | ComponentState c ->
                let element = { Element.kind = Component c._class; props = root.props |> Props.toList }
                reconcile root element
            | _ -> failwith "a mounted element at root must be a Component"

        let rootKey = ""

        let rec resolveTopPresenter mounted = 
            match mounted.state with
            | ResourceState p -> p
            | ComponentState _ -> resolveTopPresenter (mounted.nested |> Dict.toSeq |> Seq.head |> snd)


 
    type MountedRoot = 
        { mutable root: VDOM.MountedElement }
        member this.update() = this.root <- VDOM.updateRoot this.root

    let resolveTopPresenter root = VDOM.resolveTopPresenter root.root

    let mountRoot element = 
        let mounted = VDOM.mount VDOM.rootKey element
        { root = mounted }

