namespace FsReact

open System.Collections.Generic

type Properties = obj list

type ElementKind = 
    | Component of ComponentClass
    | Native of string

and Element = { kind: ElementKind; props: Properties; nested: Element list }

and Component =
    abstract render : unit -> Element
    abstract dispatchEvent : obj -> unit
    abstract _class : ComponentClass

and ComponentClass = 
    abstract createComponent : Properties -> Component

type Component<'event, 'state> =
    {
        _class : ComponentClass<'event, 'state>
        props : Props.Props;
        mutable state : 'state;
    } 
    interface Component with
        member this._class = this._class :> _
        member this.render() = this._class.render this
        member this.dispatchEvent event =
            this.state <- this._class.update this (event :?> 'event, [])

and ComponentClass<'event, 'state> =
    { 
        getInitialState : unit -> 'state; 
        getDefaultProps : unit -> Properties;
        update : Component<'event, 'state> -> 'event * Properties -> 'state;
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

    (* Components *)

    let createClass<'event, 'state>(getInitialState, handleChange, render) : ComponentClass<'event, 'state> = 
        { 
            getInitialState = getInitialState;
            getDefaultProps = fun () -> [];
            update = handleChange;
            render = render;
        }

    let element c p = { kind = Component c; props = p; nested = [] }
    let resource name p nested = { kind = Native name; props = p; nested = nested }

    (* Event Handdling *)

    type EventRoot = (Component * obj) -> unit

    let private _eventRoots = HashSet<EventRoot>()

    let registerEventRoot f =
        let r = _eventRoots.Add f
        assert(r)
        fun () -> 
            let r = _eventRoots.Remove f
            assert(r)

    let dispatchEvent event = 
        // take a copy here, _eventRoots may change while dispatching the events
        _eventRoots 
        |> Seq.toArray
        |> Array.iter (fun f -> f event)
