module InteractionTests

open Dialog
open Dialog.UI

open System

type Events = 
    | SliderChanged
    | MinimumChanged
    | MaximumChanged

type State = { slider: float; minimum: float ; maximum: float }

let rangeLimitedSlider = 

    let update this e =
        printfn "msg: %A" e.message 
        match e.message with
        | MinimumChanged ->
            { this.state with minimum = Math.Min(this.state.maximum, e.sender.get(function SliderValue v -> v))}
        | MaximumChanged ->
            { this.state with maximum = Math.Max(this.state.minimum, e.sender.get(function SliderValue v -> v))}
        | SliderChanged ->
            { this.state with slider = e.sender.get(function SliderValue v -> v) }

    let render this = 
        let state = this.state

        let minimum = state.minimum
        let maximum = state.maximum
        assert(minimum <= maximum)
        let value = this.state.slider
        let value = Math.Max(minimum, value)
        let value = Math.Min(maximum, value)

        view [
            label ("minimum: " + minimum.ToString()) []
            slider minimum (this, MinimumChanged) []
            label ("maximum: " + maximum.ToString()) []
            slider maximum (this, MaximumChanged) []
            label ("value: " + value.ToString()) []
            slider value (this, SliderChanged) []
        ] [AlignItems.Center; JustifyContent.Center; BackgroundColor Color.White]

    Define.Component()
        .Update(update)
        .Render(render)
        .InitialState({ slider = 0.5; minimum = 0.0; maximum = 1.0 })