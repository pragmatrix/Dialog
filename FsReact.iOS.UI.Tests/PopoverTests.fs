
module PopoverTests
(*

open FsReact
open FsReact.UI

type PopoverTestState = { popoverSource: Reference }

let popoverTest = 

    let initialState () = { popover = None }

    let update this (e, _) = 
        let state = this.state
        match e with 
        | ButtonClicked -> { state with count = state.count+1 }
        this.getId(



    let render this = 
        let state = this.state

        button "Show Popover" (this, ButtonClicked) []
            
    Core.createClass(initialState, update, render)

*)