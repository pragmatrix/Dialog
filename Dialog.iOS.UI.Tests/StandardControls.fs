module StandardControls

open Dialog
open Dialog.UI
open System

type Content = Content of Element
type Label = Label of string

let presenterComponent = 
    
    let render this =
        let props = this.props
        let content = props.get(fun (Content c) -> c)
        let l = props.get(fun (Label l) -> l)

        view [
            label (l+":") [Margin (Spacing.Left 16.)]
            content
        ] [LayoutDirection.Row; AlignSelf.Stretch; JustifyContent.SpaceBetween]

    Define
        .Component<unit, unit>()
        .Render(render)

let present label content = render presenterComponent [Label label; Content content]
let group l = label (l+":") [AlignSelf.Start; FontSize 20.; TextColor Color.Red]

type Events = 
    | ButtonPressed
    | SliderChanged
    | StepperChanged

let standardControls = 

    let update this e = 
        printfn "msg: %A" e.message
        match e.message with
        | ButtonPressed -> this.state
        | SliderChanged -> e.sender.get(fun (SliderValue v) -> v)
        | StepperChanged -> e.sender.get(fun (StepperValue v) -> (v |> float) / 100.)
            
    let render this = 
        let imageSource = Resource "cloud-download.png"
        let button = button "Button" (this, ButtonPressed) []
        let imageButton = imageButton imageSource (this, ButtonPressed) [Width 30.; Height 30.]
        let switch = switch On (this, ButtonPressed) []
        let slider = slider this.state (this, SliderChanged) []
        let newStepperValue = (this.state * 100. |> Math.Round |> int)
        let stepper = stepper newStepperValue 100 (this, StepperChanged) []
        let segmented = segmented [Text "One"; Text "Two"; Text "Three"] (this, ButtonPressed) []

        let label = label "Label" []
        let image = image imageSource [Width 30.; Height 30.]

        view [
            view [
                group "static"
                present "label" label
                present "image" image
                group "controls"
                present "button" button
                present "imageButton" imageButton
                present "switch" switch
                present "slider" slider
                present "stepper" stepper
                present "segmented" segmented
            ] [Width 300.]
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Define
        .Component()
        .InitialState(0.5)
        .Update(update)
        .Render(render)




