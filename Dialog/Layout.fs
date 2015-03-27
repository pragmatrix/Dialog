namespace Dialog

open Dialog.UI

open Facebook.CSSLayout
open PropertyAccessor

module Layout =

    type Convert =

        static member align align = 
            match align with
            | Align.Auto -> CSSAlign.Auto
            | Align.Start -> CSSAlign.FlexStart
            | Align.Center -> CSSAlign.Center
            | Align.End -> CSSAlign.FlexEnd
            | Align.Stretch -> CSSAlign.Stretch
        static member align align = 
            match align with
            | CSSAlign.Auto -> Align.Auto
            | CSSAlign.FlexStart -> Align.Start
            | CSSAlign.Center -> Align.Center
            | CSSAlign.FlexEnd -> Align.End
            | CSSAlign.Stretch -> Align.Stretch
            | unexpected -> failwithf "unexpected align: %A" unexpected

    let private setSpacing spacing setter = 
        setter(SpacingType.Left, spacing.left |> float32)
        setter(SpacingType.Top, spacing.top |> float32)
        setter(SpacingType.Right, spacing.right |> float32)
        setter(SpacingType.Bottom, spacing.bottom |> float32)

    let private getSpacing getter =
        let left = getter(SpacingType.Left) |> float
        let top = getter(SpacingType.Top) |> float
        let right = getter(SpacingType.Right) |> float
        let bottom = getter(SpacingType.Bottom) |> float
        { Spacing.left = left; top = top; right = right; bottom = bottom }

    let properties (getter: _ -> CSSNode) accessor = 
        let stored = Lense.store accessor

        accessor

        |> Lense.enter getter

        |> reader -- fun this -> this.AlignSelf |> Convert.align |> AlignSelf
        |> writer -- fun this (AlignSelf align) -> this.AlignSelf <- Convert.align align

        |> reader -- fun this -> this.Flex |> float |> Flex
        |> writer -- fun this (Flex f) -> this.Flex <- f |> float32

        |> reader -- fun this -> getSpacing this.GetMargin |> Margin
        |> writer -- fun this (Margin spacing) -> setSpacing spacing this.SetMargin

        |> reader -- fun this -> getSpacing this.GetBorder |> Border
        |> writer -- fun this (Border spacing) -> setSpacing spacing this.SetBorder

        |> reader -- fun this -> getSpacing this.GetPadding |> Padding
        |> writer -- fun this (Padding spacing) -> setSpacing spacing this.SetPadding

        |> reader -- fun this -> this.StyleWidth |> float |> Width
        |> writer -- fun this (Width width) -> this.StyleWidth <- width |> float32

        |> reader -- fun this -> this.StyleHeight |> float |> Height
        |> writer -- fun this (Height height) -> this.StyleHeight <- height |> float32

        |> Lense.restore stored


    let viewProperties (getter: _ -> CSSNode) accessor =
        let stored = Lense.store accessor

        accessor 
        |> Lense.enter getter

        |> reader -- 
            fun this -> 
                match this.FlexDirection with
                | CSSFlexDirection.Column -> LayoutDirection.Column
                | CSSFlexDirection.Row -> LayoutDirection.Row
                | unexpected -> failwithf "unexpected FlexDirection: %A" unexpected
        |> writer --
            fun this (direction : LayoutDirection) ->
                this.FlexDirection <-
                match direction with
                | LayoutDirection.Column -> CSSFlexDirection.Column
                | LayoutDirection.Row -> CSSFlexDirection.Row

        |> reader --
            fun this ->
                match this.JustifyContent with
                | CSSJustify.FlexStart -> JustifyContent.Start
                | CSSJustify.Center -> JustifyContent.Center
                | CSSJustify.FlexEnd -> JustifyContent.End
                | CSSJustify.SpaceBetween -> JustifyContent.SpaceBetween
                | CSSJustify.SpaceAround -> JustifyContent.SpaceAround
                | unexpected -> failwithf "unexpected JustifyContent: %A" unexpected
        |> writer --
            fun this (justify : JustifyContent) ->
                this.JustifyContent <-
                match justify with
                | JustifyContent.Start -> CSSJustify.FlexStart
                | JustifyContent.Center -> CSSJustify.Center
                | JustifyContent.End -> CSSJustify.FlexEnd
                | JustifyContent.SpaceBetween -> CSSJustify.SpaceBetween
                | JustifyContent.SpaceAround -> CSSJustify.SpaceAround

        |> reader -- fun this -> this.AlignItems |> Convert.align |> AlignItems
        |> writer -- fun this (AlignItems align) -> this.AlignItems <- Convert.align align


        |> reader --
            fun this ->
                match this.Wrap with
                | CSSWrap.NoWrap -> Wrap.NoWrap
                | CSSWrap.Wrap -> Wrap.Wrap
                | unexpected -> failwithf "unexpected Wrap: %A" unexpected
        |> writer --
            fun this (wrap : Wrap) ->
                this.Wrap <- 
                match wrap with
                | Wrap.NoWrap -> CSSWrap.NoWrap
                | Wrap.Wrap -> CSSWrap.Wrap

        |> Lense.restore stored


    type Measure = 
        | SizeThatFits of (float -> float * float)
        | MinimumSize of float * float

    type Layout(setFrame: Rect -> unit) =
        inherit CSSNode()

        let mutable _preferredSize = (0., 0.)

        member this.calculatePreferredSize() = 

            this.StyleWidth <- CSSConstants.Undefined
            this.StyleHeight <- CSSConstants.Undefined

            if (this.IsDirty) then
                this.CalculateLayout()
                if this.HasNewLayout then
                    _preferredSize <- this.LayoutWidth |> float, this.LayoutHeight |> float
                    this.MarkLayoutSeen()
            
            _preferredSize

        member this.updateTarget() = 
            if not this.HasNewLayout then ()
            else

            let left = this.LayoutX |> float
            let top = this.LayoutY |> float
            let width = this.LayoutWidth |> float
            let height = this.LayoutHeight |> float
            setFrame {left = left; top = top; width = width; height = height }
            this.MarkLayoutSeen()

            this.updateNested()

        member this.updateNested() = 
            this.Children |> Seq.iter (fun c -> (c :?> Layout).updateTarget())

        member this.layoutNested(width : float, height: float) = 
        
            if (this.Parent <> null) then () 
            else

            this.StyleWidth <- width |> float32
            this.StyleHeight <- height |> float32

            if (not this.IsDirty) then ()
            else

            this.CalculateLayout()
            if (not this.HasNewLayout) then ()
            else
            this.updateNested()
            this.MarkLayoutSeen()

        member this.insertNested index layout = 
            this.InsertChild(index, layout :> CSSNode)

        member this.removeNested (layout : Layout) = 
            layout.RemoveSelf()
            
        member this.setMeasure measure' = 
            match measure' with
            | SizeThatFits sizeOfWidth ->
                this.MeasureFunction <- 
                fun _ width ->
                    let (w, h) = sizeOfWidth (width |> float)
                    MeasureOutput(w |> float32, h |> float32)
            | MinimumSize (width, height) ->
                this.MeasureFunction <-
                fun _ _ ->
                    MeasureOutput(width |> float32, height |> float32)

            