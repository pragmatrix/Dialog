namespace Dialog

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

type Props = 
    abstract tryGet : ('property -> 'value) -> 'value option
    abstract properties : obj list
    abstract apply : Properties -> Props

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Props =

    let private key (prop:obj) = prop.GetType().Name

    type private PropsI(map : Map<string, obj>) = 
        class end
        with
        interface Props with
            member this.tryGet f =
                let key = typedefof<'property>.Name
                match Map.tryFind key map with
                | Some v -> Some (f (v :?> 'property))
                | None -> None

            member this.properties = 
                map |> Map.toSeq |> Seq.map snd |> Seq.toList

            member this.apply properties = 
                let applyOne (props: Map<_, _>) prop =
                    props.Add(key prop, prop)

                let map = 
                    properties
                    |> List.fold applyOne map

                PropsI(map) :> Props

    let ofProperties properties =
        let map = 
            properties 
            |> Seq.map (fun p -> key p, p)
            |> Map.ofSeq
        PropsI(map) :> Props

[<AutoOpen>]
module PropsExtensions =
    type Props with

        member this.tryGetOr (f : 'property -> 'value) alt = 
            match this.tryGet f with
            | Some p -> p
            | None -> alt

        member this.get (f: 'property -> 'value) = 
            match this.tryGet f with
            | Some p -> p
            | None -> failwithf "Property %A not found" (typedefof<'property>.Name)
