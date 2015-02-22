namespace FsReact

open FsReact
open FsReact.Core
open FsReact.UI
open UIKit
open Resources
open System
open CoreGraphics
open Facebook.CSSLayout
open System.Collections.Generic

module iOS =

    open VDOM

    type Control(view: UIView, css: CSSNode) = 
        member this.view = view
        member this.css = css
        member this.updateCSS() = 
            if (not css.HasNewLayout) then ()
            else
            let left = css.LayoutX
            let top = css.LayoutY
            let width = css.LayoutWidth
            let height = css.LayoutHeight
            let frame = CGRect(nfloat(left), nfloat(top), nfloat(width), nfloat(height))
            view.Frame <- frame
            css.MarkLayoutSeen()

    type Control<'view when 'view :> UIView>(view: 'view, css: CSSNode) = 
        inherit Control(view, css)
        member this.view = view



    type UICSSLayoutView(css: CSSNode) =
        inherit UIView()

        let controls = HashSet<Control>()

        override this.LayoutSubviews() = 
            if (css.Parent <> null) then ()
            else
            let bounds = this.Bounds
            css.StyleWidth <- bounds.Width |> float32
            css.StyleHeight <- bounds.Height |> float32

            if (not css.IsDirty) then ()
            else

            css.CalculateLayout()
            if (not css.HasNewLayout) then ()
            controls |> Seq.iter (fun ctrl -> ctrl.updateCSS())
            css.MarkLayoutSeen()

        member this.addControl(control: Control) = 
            controls.Add control |> ignore

        member this.removeControl(control: Control) = 
            controls.Remove control |> ignore
            

    type View(view: UICSSLayoutView, css:CSSNode) = 
        inherit Control<UICSSLayoutView>(view, css)

    let buttonWriter = 
        PropertyWriter.empty<Control<UIButton>>
        |. (Text "", fun this (Text t) -> 
            this.view.SetTitle(t, UIControlState.Normal)
            this.view.SizeToFit()

            this.css.StyleWidth <- this.view.Bounds.Width |> float |> Math.Ceiling |> float32
            this.css.StyleHeight <- this.view.Bounds.Height |> float |> Math.Ceiling |> float32
            )
        
        |+ fun this (OnClick e) -> 
            let handler = EventHandler(fun o ea -> dispatchEvent e)
            this.view.TouchUpInside.AddHandler handler
            fun () -> this.view.TouchUpInside.RemoveHandler handler


    let labelWriter = 
        PropertyWriter.empty<Control<UILabel>>
        |. (Text "", fun this (Text t) -> this.view.Text <- t)

    let viewWriter = 
        PropertyWriter.empty<View>
        |+ fun _ (Elements _) -> id
        |. (AlignItems Auto, fun this (AlignItems align) ->
            this.css.AlignItems <- 
                match align with
                | Align.Auto -> CSSAlign.Auto
                | Align.Start -> CSSAlign.FlexStart
                | Align.Center -> CSSAlign.Center
                | Align.End -> CSSAlign.FlexEnd
                | Align.Stretch -> CSSAlign.Stretch
            )
        |. (JustifyContent Start, fun this (JustifyContent justify) ->
            this.css.JustifyContent <-
                match justify with
                | Justify.Start -> CSSJustify.FlexStart
                | Justify.Center -> CSSJustify.Center
                | Justify.End -> CSSJustify.FlexEnd
                | Justify.SpaceBetween -> CSSJustify.SpaceBetween
                | Justify.SpaceAround -> CSSJustify.SpaceAround
            )

    let inline controlDisposer (v:#Control) = 
        v.view.RemoveFromSuperview()
        v.view.Dispose()

    let createControl view = 
        Control<_>(view, CSSNode())

    let createView props = 
        let css = CSSNode()
        let view = new UICSSLayoutView(css)
        let view = View(view, css)
        let mounter (this : View) (index:int) (nested : Control) = 
            this.css.InsertChild(index, nested.css)
            this.view.InsertSubview(nested.view, nint(index))
            this.view.addControl(nested)

        let unmounter (this : View) (nested : Control) = 
            this.view.removeControl(nested)
            nested.view.RemoveFromSuperview()
            nested.css.RemoveSelf()

        let nestingAdapter = 
            NestingAdapter(mounter, unmounter)

        createAncestorResource viewWriter controlDisposer nestingAdapter view props
        
    let createButton props = 
        let button = UIButton.FromType(UIButtonType.System)
        let control = createControl button
        createResource buttonWriter controlDisposer control props

    let createLabel props =
        let label = new UILabel()
        let control = createControl label
        createResource labelWriter controlDisposer control props

    let registerResources() =
        Registry.register "Button" createButton
        Registry.register "Text" createLabel
        Registry.register "View" createView

    let private dispatchEventAndUpdate (root: MountedRoot) (comp : Component, event) = 
        comp.dispatchEvent event
        root.update()
        
    let private registerEventRoot mountedRoot = 
        registerEventRoot (dispatchEventAndUpdate mountedRoot)

    let renderToController element (target : UIViewController) =
        if target.IsViewLoaded then
            failwith "FsReact can only render to an UIViewController that has no view yet."

        let mountView (controller : UIViewController) index (control:Control) = 
            assert(index = 0)
            controller.View <- control.view

        let unmountView (controller: UIViewController) (control:Control) = 
            assert(controller.View = control.view)
            controller.View <- null

        let nestingAdapter = NestingAdapter(mountView, unmountView)
        let disposer _ = ()

        let controllerResource = 
            Resources.createAncestorResource 
                PropertyWriter.empty 
                disposer 
                nestingAdapter
                target 
                []

        let controllerState = { name="UIController"; resource = controllerResource }

        let mounted = UI.mountRoot controllerState element
        registerEventRoot mounted |> ignore

(*

    let renderToView element (target : UIView) = 
        if (target.Subviews.Length <> 0) then
            failwith "React.render can only render to an UIView with no subviews"

        let mounted = UI.mountRoot element
        registerEventRoot mounted |> ignore

        let topPresenter = UI.resolveTopPresenter mounted
        target.AddSubview(topPresenter.resource.instance :?> UIView)

*)