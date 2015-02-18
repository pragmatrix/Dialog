module FsReactUI

open FsReact
open Resources
open Facebook.CSSLayout

type Elements = Elements of Element list
type Text = Text of string
type OnClick = OnClick of (Component * obj)
type Flex = Flex of int

let button text event p = native "Button" (Props.concat [Text text; OnClick event] p)
let text text p = native "Text" (Text text :> obj :: p)
let view c p = native "View" (Elements c :> obj :: p)


module VDOM = 

    type Presenter = { resource: Resource; css: CSSNode }

    type MountedState = 
        | Presenter of Presenter
        | ComponentState of Component
        with
        member this.unmount() =
            match this with
            | Presenter { resource = resource; css = css } ->
                css.RemoveSelf()
                resource.Dispose()
            | _ -> ()

    type MountedElement = 
        {
            kind: ElementKind; 
            props: Props;
            key: string; 
            state: MountedState; 
            nested: MountedElement list;
        }
        with
        member this.applyProps props =
            let newProps =
                this.props |> Props.apply props
            { this with props = newProps }

    let derivedKey key (i:int) = key + "." + (i |> string)

    let rec mount (element : Element) key = 
        match element.kind with
        | Component c ->
            let c = c.createComponent element.props
            let nested = c.render()
            let nested = mount nested (derivedKey key 0)
            { 
                kind = element.kind; 
                props = element.props |> Props.ofList; 
                key = key; 
                state = ComponentState c; 
                nested = [nested] 
            }

        | Native name ->
            let resource = Registry.createResource name element.props
            let props = element.props |> Props.ofList
            let nested = 
                props 
                |> Props.tryGetOr (function Elements nested -> nested) []
                |> List.mapi (fun i element -> mount element (derivedKey key i))
            {
                kind = element.kind;
                props = props;
                key = key;
                state = Presenter { resource = resource; css = CSSNode() };
                nested = nested
            }

    let rec unmount (mounted: MountedElement) =
        mounted.nested |> List.iter unmount
        mounted.state.unmount()

    let rec reconcile context (left: MountedElement) (right: Element) = 
        match left.kind, right.kind with
        | Component lc, Component rc when lc = rc ->
            left.applyProps right.props

        | Native ln, Native rn  when ln = rn ->
            left.applyProps right.props

        | _ ->
            unmount left
            mount right (left.key)

    let rec reconcileNested context (left: MountedElement list) (right : Element list) =
        ()

    let rootKey = ""

    let rec resolveTopPresenter mounted = 
        match mounted.state with
        | Presenter p -> p
        | ComponentState _ -> resolveTopPresenter (List.head mounted.nested)

let resolveTopPresenter = VDOM.resolveTopPresenter

let mountRoot element = VDOM.mount element VDOM.rootKey