﻿namespace FsReact

module UI =

    open FsReact.Core

    type Text = Text of string
    type OnClick = OnClick of (Component * obj)

    type Color = { red: float; green: float; blue: float; alpha: float }
        with
        static member White = { red = 1.0; green = 1.0; blue = 1.0; alpha = 1.0 }
        static member Black = { red = 0.0; green = 0.0; blue = 0.0; alpha = 1.0 }
        static member Transparent = { red = 0.0; green = 0.0; blue = 0.0; alpha = 0.0 }

    type BackgroundColor = | BackgroundColor of Color

    type Flex = Flex of int

    type Align = 
        | Auto
        | Start
        | Center
        | End
        | Stretch

    type AlignItems = AlignItems of Align
        with
        static member Auto = AlignItems Auto
        static member Start = AlignItems Start
        static member Center = AlignItems Center
        static member End = AlignItems End
        static member Stretch = AlignItems Stretch

    type Justify =
        | Start
        | Center
        | End
        | SpaceBetween
        | SpaceAround

    type JustifyContent = JustifyContent of Justify
        with
        static member Start = JustifyContent Start
        static member Center = JustifyContent Center
        static member End = JustifyContent End
        static member SpaceBetween = JustifyContent SpaceBetween
        static member SpaceAround = JustifyContent SpaceAround

    let button text event p = native "Button" (Props.concat [Text text; OnClick event] p)
    let text text p = native "Text" (Text text :> obj :: p)
    let view c p = native "View" (Elements c :> obj :: p)

    let mountRoot = VDOM.mountRoot 