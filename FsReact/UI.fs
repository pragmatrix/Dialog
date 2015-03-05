namespace FsReact

module UI =

    open Core

    type Source = 
        | Resource of string

    type Text = Text of string
    type Image = Image of Source

    type Title = Title of string

    type OnClick = OnClick of (Component * obj)
    let OnClick (c, e) = OnClick (c, box e)

    type OnDismissed = OnDismissed of (Component * obj)
    let OnDismissed (c, e) = OnDismissed (c, box e)

    type Rect = { left: float; top: float; width: float; height: float }
        with
        member this.right = this.left + this.width
        member this.bottom = this.top + this.height

    type Frame = Frame of Rect
    type Anchor = Anchor of Reference

    type Color = { red: float; green: float; blue: float; alpha: float }
        with
        static member White = { red = 1.0; green = 1.0; blue = 1.0; alpha = 1.0 }
        static member Black = { red = 0.0; green = 0.0; blue = 0.0; alpha = 1.0 }
        static member Transparent = { red = 0.0; green = 0.0; blue = 0.0; alpha = 0.0 }

    type BackgroundColor = BackgroundColor of Color

    [<AutoOpen>]
    module Layout = 

        type Align = 
            | Auto
            | Start
            | Center
            | End
            | Stretch

    type LayoutDirection = 
        | Column
        | Row

    type JustifyContent = 
        | Start
        | Center
        | End
        | SpaceBetween
        | SpaceAround

    type AlignItems = AlignItems of Align
        with
        static member Auto = AlignItems Align.Auto
        static member Start = AlignItems Align.Start
        static member Center = AlignItems Align.Center
        static member End = AlignItems Align.End
        static member Stretch = AlignItems Align.Stretch

    type AlignSelf = AlignSelf of Align
        with
        static member Auto = AlignSelf Align.Auto
        static member Start = AlignSelf Align.Start
        static member Center = AlignSelf Align.Center
        static member End = AlignSelf Align.End
        static member Stretch = AlignSelf Align.Stretch

    type Wrap = 
        | Wrap
        | NoWrap

    type Flex = Flex of int

    type Spacing = { left: float; top: float; right: float; bottom: float }
        with
        static member empty = { left= 0.; top= 0.; right= 0.; bottom= 0.}

        static member Left v = Spacing.empty.withLeft v
        static member Top v = Spacing.empty.withTop v
        static member Right v = Spacing.empty.withRight v
        static member Bottom b = Spacing.empty.withBottom b
        static member Vertical v = { left = 0.; top = v; right = 0.; bottom = v }
        static member Horizontal v = { left = v; top = 0.; right = v; bottom = 0. }
        static member All v = { left = v; top = v; right = v; bottom = v }
        
        member this.withLeft l = { this with left = l }
        member this.withTop t = { this with top = t }
        member this.withRight r = { this with right = r }
        member this.withBottom b = { this with bottom = b }
         

    type Margin = Margin of Spacing
    type Border = Border of Spacing
    type Padding = Padding of Spacing

    // position is not supported for now.
    (*
    type PositionType = 
        | Relative
        | Absolute
    *)
    // type Position = Position of Rect

    type Width = Width of float 
    type Height = Height of float

    let render = Core.render

    let button text event p = service "Button" (Properties.concat [Text text; OnClick event] p) []
    let imageButton source event p = service "Button" (Properties.concat [Image source; OnClick event] p) []
    let label text p = service "Label" (Text text :> obj :: p) []
    let image (source:Source) p = service "Image" (box source :: p) []

    let view nested p = service "View" p nested
    let popover title dismissed nested p = service "Popover" (Properties.concat [Title title; OnDismissed dismissed] p) nested


    let mountRoot = Services.mountRoot