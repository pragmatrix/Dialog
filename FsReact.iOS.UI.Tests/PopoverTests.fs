module PopoverTests

open FsReact
open FsReact.UI

type PopoverTestState = { anchor: Reference option; count: int }
type Events = 
    | ButtonClicked
    | PopoverDismissed
    | AddText

let popoverTest = 

    let initialState () = { anchor = None; count = 0 }

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with anchor = Some e.sender }
        | PopoverDismissed -> { state with anchor = None }
        | AddText -> { state with count = state.count + 1}

    let render this = 
        let state = this.state

        view [
            match state.anchor with
            | Some anchor ->
                yield popover "This is a popover" (this, PopoverDismissed) [
                    view [
                        for _ in 0..state.count do
                            yield text "Popover Content" []
                        yield button "Test Autosize" (this, AddText) []
                    ] []
                ] [Anchor anchor]
            | None -> ()
            yield button "Show Popover" (this, ButtonClicked) []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Core.createClass(initialState, update, render)
