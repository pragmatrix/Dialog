module ComponentTests

open FsReact
open FsReact.UI

type CounterEvents = | ButtonClicked
type CounterState = { count: int }

let counter = 

    let initialState () = { count = 0 }

    let update this (e, _) =
        let state = this.state
        match e with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        if (state.count % 2 = 1) then
            view [
                if ((state.count % 2) = 0) then
                    yield text ("counter: " + state.count.ToString()) []
                yield button ("Click to Count " + state.count.ToString()) (this, ButtonClicked) []
                yield button ("count: test") (this, ButtonClicked) []
            ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]
        else
            button "yoho" (this, ButtonClicked) []

//        button (sprintf "Click to count %d" state.count) (this, ButtonClicked) [];


    Core.createClass(initialState, update, render);

