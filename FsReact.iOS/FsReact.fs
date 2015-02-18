namespace FsReact

// May contain duplicates, first one wins

type Properties = obj list

type ElementKind = 
    | Component of ComponentClass
    | Native of string

and Element = { kind: ElementKind; props: Properties }

and Component =
    abstract render : unit -> Element

and ComponentClass = 
    abstract createComponent : Properties -> Component

type Component<'event, 'state> =
    {
        _class : ComponentClass<'event, 'state>
        props : Props.Props;
        state : 'state;
    } 
    interface Component with
        member this.render() = this._class.render this

and ComponentClass<'event, 'state> =
    { 
        getInitialState : unit -> 'state; 
        getDefaultProps : unit -> Properties;
        handleChange : Component<'event, 'state> -> 'event * Properties -> 'state;
        render: Component<'event, 'state> -> Element
    }
    interface ComponentClass
        with
        member this.createComponent props =
            let state = this.getInitialState()
            let defaultProps = this.getDefaultProps()
            let props = 
                defaultProps
                |> Props.ofList
                |> Props.apply props
            { _class = this; props = props; state = state } :> _

module Core =

    let createClass<'event, 'state>(getInitialState, handleChange, render) : ComponentClass<'event, 'state> = 
        { 
            getInitialState = getInitialState;
            getDefaultProps = fun () -> [];
            handleChange = handleChange;
            render = render;
        }

    let element c p = { kind = Component c; props = p }
    let native name p = { kind = Native name; props = p }
