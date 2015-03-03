module LayoutTests

open FsReact
open FsReact.UI

type CounterEvents = | ButtonClicked
type CounterState = { count: int }

let counter = 

    let initialState () = { count = 0 }

    let update this e =
        let state = this.state
        match e.message with 
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

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        if (state.count % 2 = 0) then
            button "Switch to B" (this, ButtonClicked) []
        else 
            button "Switch to A" (this, ButtonClicked) []
            

    Core.createClass(initialState, update, render)

let inline ( -- ) l r = l r

let replaceNested = 

    let initialState () = { count = 0 }

    let update this e = 
        let state = this.state
        match e.message with 
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

let twoComponents : ComponentClass<unit, unit> = 
    let initialState() = ()

    let update update this = ()

    let render this = 
        view [
            Core.element counter []
            Core.element replaceNested []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center ]


    Core.createClass(initialState, update, render)

type RectState = { rect: Rect option }

let rectFromEvent = 

    let initialState () = { rect= None }

    let update this e = 
        let state = this.state
        match e.message with 
        | ButtonClicked -> 
            let f = e.sender.get -- function Frame f -> f
            { state with rect = Some f }

    let render this = 
        let state = this.state

        view [
            button "Switch to B" (this, ButtonClicked) []
            text (sprintf "rect of button: %A"  state.rect) []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]


    Core.createClass(initialState, update, render)


let nestedViewWithLayoutChange = 

    let initialState () = { count = 0 }

    let update this e =
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        view [
            view [
                yield button ("Click") (this, ButtonClicked) []
                if this.state.count % 2 = 1 then
                    yield text ("hoi") []
            ] []
        ] [ BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]
        
    Core.createClass(initialState, update, render);