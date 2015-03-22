module ComponentTests

open Dialog
open Dialog.UI

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
        | Clicked2 -> printfn "Clicked2"
        not this.state

    let render this = 

        let content1 = label "Hello" []
        let content2 = label "IconView Component" []


        view [
            view [
                render iconView [Content content1; Image (Resource "cloud-download.png"); OnClick(this, Clicked1)]
                render iconView [Content content2; Image (Resource "cloud-download.png"); OnClick(this, Clicked2)]
            ] [Width 300.]

        ] [BackgroundColor Color.White; JustifyContent.Center; AlignItems.Start]

    Define.Component()
        .Update(update)
        .InitialState(false)
        .Render(render)

type Events2 =
    | Clicked

let dynamicContentTest = 

    let update this e =
        match e.message with
        | Clicked -> not this.state

    let render this =         
        let content1 = label "State 1" []
        let content2 = label "State 2" []
        
        let contentNow = if not this.state then content1 else content2

        view [
            render iconView [Content contentNow; Image (Resource "cloud-download.png"); OnClick(this, Clicked)]

        ] [BackgroundColor Color.White; JustifyContent.Center; AlignItems.Center]

    Define.Component()
        .Update(update)
        .Render(render)
        .InitialState(false)
