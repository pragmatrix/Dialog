namespace FsReact

open System.Collections.Generic

type Properties = obj list

type Reference = 
    abstract get : ('property -> 'value) -> 'value

[<AutoOpen>]
module ReferenceExtensions = 
    type Reference with
        static member withProperties properties = 
            {
                new Reference with
                    member this.get decon = 
                        Props.getFromList decon properties
            }

        static member empty = Reference.withProperties []

type Event<'event> = { message: 'event; props: Properties; sender: Reference }
    with 
    member this.unboxed() = { message = this.message |> unbox; props = this.props; sender = this.sender }

type ElementKind = 
    | Component of ComponentClass
    | Native of string

and Element = { kind: ElementKind; props: Properties; nested: Element list }

and Component =
    abstract render : unit -> Element
    abstract dispatchEvent : Event<obj> -> unit
    abstract _class : ComponentClass

and ComponentClass = 
    abstract createComponent : Properties -> Component

type Component<'event, 'state> =
    {
        _class : ComponentClass<'event, 'state>
        props : Props;
        mutable state : 'state;
    } 
    interface Component with
        member this._class = this._class :> _
        member this.render() = this._class.render this
        member this.dispatchEvent event =
            this.state <- this._class.update this (event.unboxed())

and ComponentClass<'event, 'state> =
    { 
        getInitialState : unit -> 'state; 
        getDefaultProps : unit -> Properties;
        update : Component<'event, 'state> -> Event<'event> -> 'state;
        render: Component<'event, 'state> -> Element
    }
    member this.GetInitialState(initial) = { this with getInitialState = initial }
    member this.InitialState(initial) = { this with getInitialState = fun () -> initial }
    member this.Render(render) = { this with render = render }
    member this.Update(update) = { this with update = update }

    interface ComponentClass with
        member this.createComponent props =
            let state = this.getInitialState()
            let defaultProps = this.getDefaultProps()
            let props = 
                defaultProps
                |> Props.ofList
                |> Props.apply props
            { _class = this; props = props; state = state } :> _

type Define =

    static member Component<'e, 's>() = 
        { 
            getInitialState = fun () -> Unchecked.defaultof<'s>
            getDefaultProps = fun () -> [];
            update = fun (c:Component<'e, 's>) _ -> c.state;
            render = fun (c:Component<'e, 's>) -> 
                failwith "render function not implemented";
        }

module Core =

    let render c p = { kind = ElementKind.Component c; props = p; nested = [] }
    let service name p nested = { kind = Native name; props = p; nested = nested }

module Events =
    (* Event Handling *)

    type EventRoot = Component -> Event<obj> -> unit

    let private _eventRoots = HashSet<EventRoot>()

    let registerEventRoot f =
        let r = _eventRoots.Add f
        assert(r)
        fun () -> 
            let r = _eventRoots.Remove f
            assert(r)

    let dispatchEvent target (e : Event<obj>) = 
        // take a copy here, _eventRoots may change while dispatching the events
        _eventRoots 
        |> Seq.toArray
        |> Array.iter (fun f -> f target e)
