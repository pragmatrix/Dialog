namespace FsReact

type PropertyWriter<'target> = { map: Map<string, 'target -> obj -> unit> }
    with 

    static member (|+) (l: PropertyWriter<'target>, f: 'target -> 'property -> unit) = 
        let name = typedefof<'property>.Name
        let apply target (property:obj) = 
            let p = property :?> 'property 
            f target p
        { l with map = l.map.Add(name, apply) }
    
    member this.write target props = 
        let write prop =
            let name = prop.GetType().Name
            let f = this.map.[name]
            f target prop

        props
        |> List.iter write

    member this.derived() : PropertyWriter<'derived> = 
        let dMap = 
            this.map
            |> Map.map (fun _ f -> (fun (d:'derived) p -> f (d :> obj :?> 'target) p))
        { map = dMap }

module PropertyWriter =

    let empty<'target> : PropertyWriter<'target> = { map = Map.empty }
