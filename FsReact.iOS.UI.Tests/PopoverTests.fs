module PopoverTests

open FsReact
open FsReact.UI

type PopoverTestState = { anchor: Reference option }
type Events = 
    | ButtonClicked
    | PopoverDismissed

let popoverTest = 

    let initialState () = { anchor = None }

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with anchor = Some e.sender }
        | PopoverDismissed -> { state with anchor = None }

    let render this = 
        let state = this.state

        view [
            match state.anchor with
            | Some anchor ->
                yield popover "This is a popover" (this, PopoverDismissed) [
                    text "Popover Content" []
                ] [Anchor anchor]
            | None -> ()
            yield button "Show Popover" (this, ButtonClicked) []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Core.createClass(initialState, update, render)
