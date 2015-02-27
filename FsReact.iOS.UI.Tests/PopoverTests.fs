module PopoverTests

open FsReact
open FsReact.UI

type PopoverTestState = { anchor: Reference option }
type ButtonClicked = | ButtonClicked

let popoverTest = 

    let initialState () = { anchor = None }

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with anchor = Some e.sender }

    let render this = 
        let state = this.state

        view [
            yield button "Show Popover" (this, ButtonClicked) []
            match state.anchor with
            | Some anchor ->
                yield popover "This is a popover" [
                    text "Popover Content" []
                ] [Anchor anchor]
            | None -> ()
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]

    Core.createClass(initialState, update, render)
