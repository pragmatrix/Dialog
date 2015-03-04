namespace FsReact

type Props = Map<string, obj>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Props =

    type Properties = obj list

    let empty : Props = Map.empty

    let private key (prop:obj) = prop.GetType().Name

    let apply (newProps : Properties) props = 

        let applyOne (props : Props) prop = 
            props.Add(key prop, prop)

        newProps
        |> List.fold applyOne props

    let ofList props = empty |> apply props
    let toList (props : Props) = props |> Map.toSeq |> Seq.map snd |> Seq.toList

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

    let tryGetFromList (f: 'property -> 'value) (props: Properties) =
        let rec tg (props : Properties) = 
            match props with
            | [] -> None
            | x::xs ->
            match x with
            | :? 'property as p -> f p |> Some
            | _ -> tg xs

        tg props

    let getFromList (f: 'property -> 'value) (props: Properties) =
        match tryGetFromList f props with
        | Some p -> p
        | None ->         
        failwithf "Property %A not found" (typedefof<'property>.Name)
