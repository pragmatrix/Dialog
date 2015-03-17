namespace Dialog

type Key = Key of string

type MountedRoot = 
    abstract update : unit -> unit
    
module ComponentDOM = 

    open Reconciler

    let mkIdentity name key = (name, key)

    type MountedState = 
        | ServiceState of ServiceRef * Properties
        | ComponentState of Component

    type MountedElement = 
        {
            key: string; 
            state: MountedState; 
            nested: Dict<string, MountedElement>;
            orderedKeys: string list;
        }
        with
        member this.updateProperties properties =
            match this.state with
            | ServiceState (name, _) ->
                { this with state = ServiceState (name, properties) }
            | _ -> this

        member this.identity =
            match this.state with
            | ServiceState (serviceRef, _) -> mkIdentity serviceRef.name this.key
            | ComponentState _ -> mkIdentity "[component]" this.key


    let derivedKey key (i:int) = key + "." + (i |> string)

    let elementKey key i (element:Element) = 
        match Properties.tryGet (function Key key -> key) element.properties with
        | Some key -> key
        | None -> derivedKey key i

    let rec mount (key:string) (element : Element) : MountedElement = 

        let state, nested = 
            match element.kind with
            | Component c ->
                let c = c.createComponent element.properties
                Trace.renderingComponent "" key

                ComponentState c, [c.render()]

            | Service name ->
                ServiceState (name, element.properties), element.nested

        let mounted = 
            {
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
        | ComponentState c, Component eClass when obj.ReferenceEquals(c.class', eClass) ->
            let mounted = mounted.updateProperties element.properties
            Trace.renderingComponent "" mounted.key
            let nested = c.render()
            reconcileNested mounted [nested]

        | ServiceState (serviceRef, _), Service serviceRef' when obj.ReferenceEquals(serviceRef, serviceRef') ->
            let mounted = mounted.updateProperties element.properties
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
            insert = fun _ -> mount;
            update = fun _ _ -> reconcile;
            remove = fun _ -> unmount;
            }

        let newNested = Reconciler.reconcile functions mounted.nested keyedNested
        { mounted with nested = newNested; orderedKeys = keyedNested |> List.map fst }

