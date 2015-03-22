module StandardControls

open Dialog
open Dialog.UI
open System
open Shared

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
    | Event
    | SliderChanged
    | StepperChanged

let standardControls = 

    let update this e = 
        printfn "msg: %A" e.message
        match e.message with
        | Event -> this.state
        | SliderChanged -> e.sender.get(fun (SliderValue v) -> v)
        | StepperChanged -> e.sender.get(fun (StepperValue v) -> (v |> float) / 100.)
            
    let render this = 
        let imageSource = Resource "cloud-download.png"
        
        let label = label "Label" []
        let image = image imageSource [Width 30.; Height 30.]

        let button = button "Button" (this, Event) []
        let imageButton = imageButton imageSource (this, Event) [Width 30.; Height 30.]
        let switch = switch On (this, Event) []
        let slider = slider this.state (this, SliderChanged) []
        let newStepperValue = (this.state * 100. |> Math.Round |> int)
        let stepper = stepper newStepperValue 100 (this, StepperChanged) []
        let segmented = segmented [Text "One"; Text "Two"; Text "Three"] (this, Event) []

        let entryNormal = entry (this, Event) []
        let entrySecure = entry (this, Event) [Secure]

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
                group "text"
                present "entry" entryNormal
                present "entry Secure" entrySecure
            ] [Width 300.]
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Define
        .Component()
        .InitialState(0.5)
        .Update(update)
        .Render(render)




