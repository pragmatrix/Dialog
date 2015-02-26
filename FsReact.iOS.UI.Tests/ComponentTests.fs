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
            text ("counter: " + state.count.ToString()) []
            button ("Click to Count ") (this, ButtonClicked) []
        ] [ BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]
        
    Core.createClass(initialState, update, render);

let replaceRoot = 

    let initialState () = { count = 0 }

    let update this (e, _) = 
        let state = this.state
        match e with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        if (state.count % 2 = 0) then
            button "Switch to B" (this, ButtonClicked) []
        else 
            button "Switch to A" (this, ButtonClicked) []
            

    Core.createClass(initialState, update, render)


let replaceNested = 

    let initialState () = { count = 0 }

    let update this (e, _) = 
        let state = this.state
        match e with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        view [
            if (state.count % 2 = 0) then
                yield button "Switch to B" (this, ButtonClicked) []
            else 
                yield button "Switch to A" (this, ButtonClicked) []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]


    Core.createClass(initialState, update, render)
