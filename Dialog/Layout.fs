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