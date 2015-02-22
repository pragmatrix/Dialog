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

        view [
            button ("Click to Count " + state.count.ToString()) (this, ButtonClicked) [];
            button ("count: " + state.count.ToString()) (this, ButtonClicked) [];
        ] [AlignItems.Center; JustifyContent.Center]

//        button (sprintf "Click to count %d" state.count) (this, ButtonClicked) [];


    Core.createClass(initialState, update, render);

