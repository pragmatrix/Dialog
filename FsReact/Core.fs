namespace FsReact

open System.Collections.Generic

type Reference = 
    abstract get : ('property -> 'value) -> 'value

[<AutoOpen>]
module ReferenceExtensions = 
    type Reference with
        static member withProperties properties = 
            {
                new Reference with
                    member this.get decon = 
                        Properties.get decon properties
            }

        static member empty = Reference.withProperties []

type Event<'event> = { message: 'event; props: Properties; sender: Reference }
    with 
    member this.unboxed() = { message = this.message |> unbox; props = this.props; sender = this.sender }

type ElementKind = 
    | Component of ComponentClass
    | Service of string

and Element = { kind: ElementKind; properties: Properties; nested: Element list }

and Component =
    abstract render : unit -> Element
    abstract dispatchEvent : Event<obj> -> unit
    abstract class' : ComponentClass

and ComponentClass = 
    abstract createComponent : Properties -> Component

type Component<'state, 'event> =
    {
        class' : ComponentClass<'state, 'event>
        props : Props;
        mutable state : 'state;
    } 
    interface Component with
        member this.class' = this.class' :> _
        member this.render() = this.class'.render this
        member this.dispatchEvent event =
            this.state <- this.class'.update this (event.unboxed())

and ComponentClass<'state, 'event> =
    { 
        getInitialState : unit -> 'state; 
        getDefaultProperties : unit -> Properties;
        update : Component<'state, 'event> -> Event<'event> -> 'state;
        render: Component<'state, 'event> -> Element
    }
    member this.GetInitialState(initial) = { this with getInitialState = initial }
    member this.InitialState(initial) = { this with getInitialState = fun () -> initial }
    member this.Render(render) = { this with render = render }
    member this.Update(update) = { this with update = update }

    interface ComponentClass with
        member this.createComponent props =
            let state = this.getInitialState()
            let defaultProperties = this.getDefaultProperties()
            let props = 
                defaultProperties
                |> Props.ofProperties
                |> Props.apply props
            { class' = this; props = props; state = state } :> _

type Define =

    static member Component<'s, 'e>() = 
        { 
            getInitialState = fun () -> Unchecked.defaultof<'s>
            getDefaultProperties = fun () -> [];
            update = fun (c:Component<'s, 'e>) _ -> c.state;
            render = fun (_:Component<'s, 'e>) -> 
                failwith "render function not implemented";
        }

module Core =

    let render c p = { kind = ElementKind.Component c; properties = p; nested = [] }
    let service name p nested = { kind = Service name; properties = p; nested = nested }

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
