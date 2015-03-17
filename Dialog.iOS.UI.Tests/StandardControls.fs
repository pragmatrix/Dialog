module StandardControls

open Dialog
open Dialog.UI

type Events = 
    | Event

type Content = Content of Element
type Label = Label of string


let presenterComponent = 
    
    let render this =
        let props = this.props
        let content = props.get(function Content c -> c)
        let l = props.get(function Label l -> l)

        view [
            label (l+":") [Margin (Spacing.Left 16.)]
            content
        ] [LayoutDirection.Row; AlignSelf.Stretch; JustifyContent.SpaceBetween]

    Define
        .Component<unit, unit>()
        .Render(render)

let present label content = render presenterComponent [Label label; Content content]
let group l = label l [AlignSelf.Start; FontSize 20.; TextColor Color.Red]

let standardControls = 

    let update this e = 
        match e.message with
        | Event -> this.state

    let render this = 
        let imageSource = Resource "cloud-download.png"

        let button = button "Button" (this, Event) []
        let imageButton = imageButton imageSource (this, Event) [Width 30.; Height 30.]
        let switch = switch On (this, Event) []

        let label = label "Label" []
        let image = image imageSource [Width 30.; Height 30.]

        view [
            view [
                group "passive"
                present "label" label
                present "image" image
                group "controls"
                present "button" button
                present "imageButton" imageButton
                present "switch" switch
            ] [Width 300.]
        ] [BackgroundColor Color.White; AlignItems.Center]

    Define
        .Component()
        .Update(update)
        .Render(render)
        .InitialState(0)




