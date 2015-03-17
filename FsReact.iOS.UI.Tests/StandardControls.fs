module StandardControls

open FsReact
open FsReact.UI

type Events = 
    | Events

let standardControls = 

    let update this e = 
        match e.message with
        | Events -> this.state

    let render this = 
        view [] [BackgroundColor Color.White]

    Define
        .Component()
        .Update(update)
        .Render(render)
        .InitialState(0)




