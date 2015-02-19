namespace FsReact

(* 
    Defines property modification functions for a specific target

    |+ provides an "add" function, which is called when a property is set the first time, or updated if no |* function is set
    |* provides an "update" function, which is called whenver the property is changed
    |- provides a "remove" fucntion, which is called whenever the property is removed

    todo:
        - use Dictionaries instead of maps for performance
        - pre-build updateMap so that it contains also the adders
        - swap target obj signature, so that we specialize over properties?
        - add a |- operator that just expects a function that returns another property, which will be set
          to the update function to remove the property. (or allow independent specification of default values?)
        - we might need readers to read out properties directly from the components, PropertyWriter should be
          then renamed to PropertyAccessor?
*)

type PropertyWriter<'target> = 
    { 
        addMap: Map<string, 'target -> obj -> unit>;
        updateMap: Map<string, 'target -> obj -> unit>;
        removeMap: Map<string, 'target -> obj -> unit>;
    }
    with 

    static member (|+) (l: PropertyWriter<'target>, f: 'target -> 'property -> unit) = 
        let name = typedefof<'property>.Name
        let adder target (property:obj) = 
            let p = property :?> 'property 
            f target p
        { l with addMap = l.addMap.Add(name, adder) }

    static member (|*) (l: PropertyWriter<'target>, f: 'target -> 'property -> unit) = 
        let name = typedefof<'property>.Name
        let updater target (property:obj) = 
            let p = property :?> 'property 
            f target p
        { l with updateMap = l.updateMap.Add(name, updater) }
    
    static member (|-) (l: PropertyWriter<'target>, f: 'target -> 'property -> unit) = 
        let name = typedefof<'property>.Name
        let remover target (property:obj) = 
            let p = property :?> 'property 
            f target p
        { l with removeMap = l.removeMap.Add(name, remover) }


    member this.add target property =
        let name = property.GetType().Name
        this.addMap.[name] target property

    member this.update target property =
        let name = property.GetType().Name
        match this.updateMap.TryFind name with
        | Some updater -> updater target property
        | None ->
        this.addMap.[name] target property

    member this.remove target property = 
        let name = property.GetType().Name
        this.removeMap.[name] target property

(*
    member this.derived() : PropertyWriter<'derived> = 
        let dMap = 
            this.map
            |> Map.map (fun _ f -> (fun (d:'derived) p -> f (d :> obj :?> 'target) p))
        { map = dMap }
*)

module PropertyWriter =

    let empty<'target> : PropertyWriter<'target> = { addMap = Map.empty; updateMap = Map.empty; removeMap = Map.empty }
