module Dialog.Tests.LayoutTests

open Dialog
open Dialog.UI

type CounterEvents = | ButtonClicked
type CounterState = { count: int }

let counter = 

    let update this e =
        let state = this.state
        match e.message with 
        | ButtonClicked -> { state with count = state.count+1 }

    let render this = 
        let state = this.state

        view [
            label ("counter: " + state.count.ToString()) []
            button ("Click to Count ") (this, ButtonClicked) []
        ] [ BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]
        
    Define.Component()
        .InitialState({ count = 0 })
        .Update(update)
        .Render(render)
        
let replaceRoot = 

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

    Define.Component()
        .InitialState({count = 0})
        .Update(update)
        .Render(render)
        
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

    Define.Component()
        .InitialState({count=0})
        .Update(update)
        .Render(render)
        
let twoComponents : ComponentClass<unit, unit> = 

    let update update this = ()

    let render this = 
        view [
            render counter []
            render replaceNested []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center ]

    Define.Component()
        .InitialState(())
        .Update(update)
        .Render(render)

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
            label (sprintf "rect of button: %A"  state.rect) []
        ] [BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]


    Define.Component()
        .InitialState({ rect = None })
        .Update(update)
        .Render(render)
        
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
                    yield label ("hoi") []
            ] []
        ] [ BackgroundColor Color.White; AlignItems.Center; JustifyContent.Center]
        
    Define.Component()
        .GetInitialState(initialState)
        .Update(update)
        .Render(render)

(* CSS Layout Problems *)

let centeredItemsDoNotUseSpaceBetweenInRowDirection = 
    
    let render this =

        let block = view [] [Width 20.; Height 20.; BackgroundColor Color.Black]

        view [
            view [
                view [] [BackgroundColor Color.Black; Width 300.; Height 1.]
                view [
                    block 
                    block 
                    block
                ] [LayoutDirection.Row; JustifyContent.SpaceBetween]
                view [] [BackgroundColor Color.Black; Width 300.; Height 1.]
            ] []
        ] [AlignItems.Center; JustifyContent.Center; BackgroundColor Color.White]

    Define.Component<unit, unit>()
        .InitialState(())
        .Render(render)