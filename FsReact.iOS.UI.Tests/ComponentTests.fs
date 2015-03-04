module ComponentTests

open FsReact
open FsReact.UI

(*
    A view with an image button right next to it.
*)

type Content = Content of Element

let iconView = 

    let render this = 
        let props = this.props
        let content = props.get (function Content c -> c)
        let image = props.get (function Image i -> i)
        let onClick = props.get (function OnClick oc -> oc)

        view [
            content
            view [] [Width 10.]
            imageButton image onClick [Width 30.; Height 30.]
        ] [LayoutDirection.Row; JustifyContent.SpaceBetween]

    Define.Component<unit, unit>()
        .Render(render)

type Events = 
    | Clicked1
    | Clicked2

let iconViewTest = 
    let update this e =
        match e.message with
        | Clicked1 -> printfn "Clicked1"
        | Clicked2 -> printfn "Clicked1"
        this.state

    let render this = 

        let content1 = label "Hello" []
        let content2 = label "IconView Component" []

        view [
            view [
                render iconView [Content content1; Image (Resource "Download From Cloud-100.png"); OnClick(this, Clicked1 |> box)]
                render iconView [Content content2; Image (Resource "Download From Cloud-100.png"); OnClick(this, Clicked2 |> box)]
            ] [Width 300.]

        ] [BackgroundColor Color.White; JustifyContent.Center; AlignItems.Start]

    Define.Component()
        .Update(update)
        .InitialState(10)
        .Render(render)
                