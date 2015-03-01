namespace FsReact

open System

type Key = Key of string

type MountedRoot = 
    abstract update : unit -> unit
    
module VDOM = 

    open Reconciler

    type Identity = string * string 
    let mkIdentity name key = (name, key)

    type Props = Props.Props

    type MountedState = 
        | NativeState of string
        | ComponentState of Component

    type MountedElement = 
        {
            key: string; 
            props: Props;
            state: MountedState; 
            nested: Dict<string, MountedElement>;
            orderedKeys: string list;
        }
        with
        member this.applyProps props =
            let newProps =
                this.props |> Props.apply props
            { this with props = newProps }

        member this.identity =
            match this.state with
            | NativeState name -> mkIdentity name this.key
            | ComponentState _ -> mkIdentity "[component]" this.key


    let derivedKey key (i:int) = key + "." + (i |> string)

    let elementKey key i (element:Element) = 
        match Props.tryGetFromList (function Key key -> key) element.props with
        | Some key -> key
        | None -> derivedKey key i

    let rec mount (key:string) (element : Element) : MountedElement = 

        let state, nested = 
            match element.kind with
            | Component c ->
                let c = c.createComponent element.props
                Trace.renderingComponent "" key

                ComponentState c, [c.render()]

            | Native name ->
                NativeState name, element.nested

        let mounted = 
            {
                props = element.props |> Props.ofList;
                key = key;
                state = state;
                nested = Dict.ofList [];
                orderedKeys = [];
            }

        reconcileNested mounted nested

    and unmount (mounted: MountedElement) =
        reconcileNested mounted []
        |> ignore // just forget it

    and reconcile (mounted: MountedElement) (element: Element) = 
        match mounted.state, element.kind with
        | ComponentState c, Component eClass when obj.ReferenceEquals(c._class, eClass) ->
            let mounted = mounted.applyProps element.props
            Trace.renderingComponent "" mounted.key
            let nested = c.render()
            reconcileNested mounted [nested]

        | NativeState ln, Native rn  when ln = rn ->
            let mounted = mounted.applyProps element.props
            let nested = element.nested
            reconcileNested mounted nested

        | _ ->
            unmount mounted
            mount mounted.key element

    and reconcileNested (mounted: MountedElement) (nested: Element list) =
        let key = mounted.key

        let keyedNested = 
            nested 
            |> List.mapi (fun i element -> elementKey key i element, element)

        let functions = {
            add = fun i k e -> mount k e;
            update = fun i k m e -> reconcile m e;
            remove = fun k m -> unmount m
            }

        let newNested = Reconciler.reconcile functions mounted.nested keyedNested
        { mounted with nested = newNested; orderedKeys = keyedNested |> List.map fst }

