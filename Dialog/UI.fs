namespace Dialog

module UI =

    open System
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

        // http://www.procato.com/rgb+index/

        static member Red = Color.fromHSV(0., 0.75, 1.)
        static member Orange = Color.fromHSV(30., 0.75, 1.)
        static member Yellow = Color.fromHSV(60., 0.75, 1.)
        static member ChartreuseGreen = Color.fromHSV(90., 0.75, 1.)
        static member Green = Color.fromHSV(120., 0.75, 1.)
        static member SpringGreen = Color.fromHSV(150., 0.75, 1.)
        static member Cyan = Color.fromHSV(180., 0.75, 1.)
        static member Azure = Color.fromHSV(210., 0.75, 1.)
        static member Blue = Color.fromHSV(240., 0.75, 1.)
        static member Violet = Color.fromHSV(270., 0.75, 1.)
        static member Magenta = Color.fromHSV(300., 0.75, 1.)
        static member Rose = Color.fromHSV(330., 0.75, 1.)

        // http://stackoverflow.com/questions/359612/how-to-change-rgb-color-to-hsv

        static member fromHSV(hue: float, saturation: float, value: float) = 
            let hi = Convert.ToInt32(Math.Floor(hue / 60.)) % 6
            let f = hue / 60. - Math.Floor(hue / 60.)

            let value = value * 255.
            let v = value;
            let p = value * (1. - saturation)
            let q = value * (1. - f * saturation)
            let t = value * (1. - (1. - f) * saturation)

            let toB r g b = { red = r / 255.; green = g / 255.; blue = b / 255.; alpha = 1.0 }

            match hi with
            | 0 -> toB v t p
            | 1 -> toB q v p
            | 2 -> toB p v t
            | 3 -> toB p q v
            | 4 -> toB t p v
            | _ -> toB v p q
            

    type BackgroundColor = BackgroundColor of Color
    type TextColor = TextColor of Color

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

    let controlType = Define.ServiceType("Control")
    let controllerType = Define.ServiceType("Controller")

    let buttonService = Define.ServiceRef("Button", controlType)
    let labelService = Define.ServiceRef("Label", controlType)
    let imageService = Define.ServiceRef("Image", controlType)
    let viewService = Define.ServiceRef("View", controlType)
    let popoverService = Define.ServiceRef("Popover", controllerType)

    let button text event p = service buttonService (Properties.concat [Text text; OnClick event] p) []
    let imageButton source event p = service buttonService (Properties.concat [Image source; OnClick event] p) []
    let label text p = service labelService (Text text :> obj :: p) []
    let image (source:Source) p = service imageService (box source :: p) []

    let view nested p = service viewService p nested
    let popover title dismissed nested p = service popoverService (Properties.concat [Title title; OnDismissed dismissed] p) nested


    let mountRoot = Services.mountRoot