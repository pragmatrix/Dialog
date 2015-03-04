namespace FsReact

type Properties = obj list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Properties =

    let concat (a:Properties) b = a @ b

    let tryGet (f: 'property -> 'value) (props: Properties) =
        let rec tg (props : Properties) = 
            match props with
            | [] -> None
            | x::xs ->
            match x with
            | :? 'property as p -> f p |> Some
            | _ -> tg xs

        tg props

    let get (f: 'property -> 'value) (props: Properties) =
        match tryGet f props with
        | Some p -> p
        | None ->         
        failwithf "Property %A not found" (typedefof<'property>.Name)

type Props = Map<string, obj>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Props =

    let empty : Props = Map.empty

    let private key (prop:obj) = prop.GetType().Name

    let apply (newProps : Properties) props = 

        let applyOne (props : Props) prop = 
            props.Add(key prop, prop)

        newProps
        |> List.fold applyOne props

    let ofProperties properties = empty |> apply properties
    let toProperties (props : Props) = props |> Map.toSeq |> Seq.map snd |> Seq.toList

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
