namespace FsReact

module Props =

    type Props = Map<string, obj>
    type Properties = obj list

    let empty : Props = Map.empty

    let private key (prop:'t) = typedefof<'t>.Name

    let apply (newProps : Properties) props = 
        let applyOne (props : Props) prop = 
            props.Add(key prop, prop)

        newProps
        |> List.fold applyOne props

    let ofList props = empty |> apply props

    let tryGet (f : 'property -> 'value) (props:Props) = 
        let key = typedefof<'property>.Name
        match Map.tryFind key props with
        | Some v -> Some (f (v :?> 'property))
        | None -> None

    let tryGetOr (f : 'property -> 'value) alt props = 
        match tryGet f props with
        | Some p -> p
        | None -> alt

    let get (f: 'property -> 'value) props = 
        match tryGet f props with
        | Some p -> p
        | None -> failwithf "Property %A not found" (typedefof<'property>.Name)

    let concat (a:Properties) b = a @ b
