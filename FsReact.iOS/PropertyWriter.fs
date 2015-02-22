namespace FsReact

(* 
    Defines property modification functions for a specific target


    |. provides a default property and a setter. The setter is used to mount and update a property and the
        default property is set when it is removed.
    |+ provides an "add" function, which is called when a property is mounted and returns
        a remove function. Updates are processed by removing and adding.
    |* provides an "add" function, which is called when a property is set the first time
        and returns an update and remove function.
        
    todo:
        - use Dictionaries instead of maps for performance
        - we might need readers to read out properties directly from the components, PropertyWriter should be
          then renamed to PropertyAccessor?
        - default values should also be set when the target is instantiated.
*)

type MountedProperty<'target> = 
    abstract update: (obj -> unit) option
    abstract remove: unit -> unit

type MountedProperty<'target, 'property> = { update: ('property -> unit) option; remove: unit -> unit }
    with
    interface MountedProperty<'target> with
        member this.update =
            this.update
            |> Option.map(fun f -> fun p -> f (p :?> 'property))

        member this.remove() = this.remove()

type PropertyWriter<'target> = 
    { 
        map: Map<string, 'target -> obj -> MountedProperty<'target> >;
    }
    with 
        
    static member (|*) (l: PropertyWriter<'target>, f: 'target -> 'property -> ('property -> unit) * (unit -> unit)) = 
        let name = typedefof<'property>.Name
        let adder target (property:obj) = 
            let p = property :?> 'property 
            let (u, r) = f target p
            
            {
                update = Some u
                remove = r
            } 
            :> MountedProperty<_>

        { l with map = l.map.Add(name, adder) }

    static member (|+) (l: PropertyWriter<'target>, f: 'target -> 'property -> (unit -> unit)) = 
        let name = typedefof<'property>.Name
        let adder target (property:obj) = 
            let p = property :?> 'property 
            let r = f target p
            {
                update = None
                remove = r
            } 
            :> MountedProperty<_>

        { l with map = l.map.Add(name, adder) }

    static member (|.) (l: PropertyWriter<'target>, arg : 'property * ('target -> 'property -> unit)) = 
        let def, setter = arg
        l
        |* fun t p ->
            let set = setter t
            set p
            let update p = set p
            let remove() = set def
            update, remove

    member this.mount target property =
        let name = property.GetType().Name
        match this.map.TryFind name with
        | Some p -> p target property
        | None ->
            failwithf "native type '%s' does not support property '%s'" typedefof<'target>.Name name

(*
    member this.derived() : PropertyWriter<'derived> = 
        let dMap = 
            this.map
            |> Map.map (fun _ f -> (fun (d:'derived) p -> f (d :> obj :?> 'target) p))
        { map = dMap }
*)

module PropertyWriter =

    let empty<'target> : PropertyWriter<'target> = { map = Map.empty }
