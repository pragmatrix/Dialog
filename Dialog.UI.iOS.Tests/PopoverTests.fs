module PopoverTests

open Dialog
open Dialog.UI

type PopoverTestState = { anchor: Reference option; count: int }
type Events = 
    | ButtonClicked
    | PopoverDismissed
    | AddText

let popoverTest = 

    let initialState = { anchor = None; count = 0 }

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with anchor = Some e.sender }
        | PopoverDismissed -> { state with anchor = None }
        | AddText -> { state with count = state.count + 1}

    let render this = 
        let state = this.state

        view [
            view [
                match state.anchor with
                | Some anchor ->
                    yield popover "This is a popover" (this, PopoverDismissed) [
                        view [
                            view [
                                for _ in 0..state.count do
                                    yield label "Popover Content" []
                                yield button "Test Autosize" (this, AddText) []
                            ] []
                        ] [Margin (Spacing.All 10.)]
                    ] [Anchor anchor]
                | None -> ()
                yield button "Show Popover" (this, ButtonClicked) []
                for _ in 0..state.count do
                    yield label "Autosize Test" []
                yield button ("Test Autosize "+state.count.ToString()) (this, AddText) []

                // Icon by icons8.com
                yield image (Resource "cloud-download.png") [Width 30.; Height 30.]
                yield imageButton (Resource "cloud-download.png") (this, AddText) [Width 30.; Height 30.]
                yield view [] [BackgroundColor Color.Black; AlignSelf.Stretch; Height 1.]
            ] [Width 300.]
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Define.Component()
        .InitialState(initialState)
        .Update(update)
        .Render(render)
        