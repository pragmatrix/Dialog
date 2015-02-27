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

    type Convert = 
        static member rect (r: CGRect) = 
            { left = r.X |> float; top = r.Y |> float; width = r.Width |> float; height = r.Height |> float }

    open VDOM

    type Control(view: UIView, css: CSSNode) = 
        member this.view = view
        member this.css = css
        abstract member updateCSS: unit -> unit

        default this.updateCSS() = 
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
            this.updateCSS()
            css.MarkLayoutSeen()

        member this.updateCSS() =
            controls |> Seq.iter (fun ctrl -> ctrl.updateCSS())
            
        member this.addControl(control: Control) = 
            controls.Add control |> ignore

        member this.removeControl(control: Control) = 
            controls.Remove control |> ignore
            

    type View(view: UICSSLayoutView, css:CSSNode) = 
        inherit Control<UICSSLayoutView>(view, css)
        override this.updateCSS() =
            base.updateCSS()
            view.updateCSS()

    open PropertyAccessor

    let mkHandler handler = EventHandler handler
    let mkEvent target msg reader = { message = msg; props = []; sender = ResourceReference(target, reader) }

    let buttonReader = 
        readerFor<Control<UIButton>>
        |> reader --
            fun this -> this.view.Frame |> Convert.rect |> Frame

    let buttonAccessor = 
        accessor<Control<UIButton>>
        |> defaultValue -- Text ""
        |> writer --
            fun this (Text t) ->
                UIView.PerformWithoutAnimation(
                    fun _ ->
                        this.view.SetTitle(t, UIControlState.Normal)
                        this.view.LayoutIfNeeded())

                this.view.SizeToFit()

                this.css.StyleWidth <- this.view.Bounds.Width |> float32
                this.css.StyleHeight <- this.view.Bounds.Height |> float32
        |> mounter --
            fun this (OnClick (comp, msg)) ->
                let handler = mkHandler -- fun _ _ -> dispatchEvent comp -- mkEvent this msg buttonReader
                this.view.TouchUpInside.AddHandler handler
                fun () -> this.view.TouchUpInside.RemoveHandler handler

    let labelAccessor = 
        accessor<Control<UILabel>>
        |> defaultValue -- Text ""
        |> writer --
            fun this (Text t) -> 
                this.view.Text <- t
                this.view.SizeToFit()
                this.css.StyleWidth <- this.view.Bounds.Width |> float32
                this.css.StyleHeight <- this.view.Bounds.Height |> float32

    let viewAccessor = 
        accessor<View>
        |> defaultValue -- BackgroundColor Color.Transparent
        |> defaultValue -- AlignItems Auto
        |> defaultValue -- JustifyContent Start
        |> writer --
            fun this (BackgroundColor color) ->
                this.view.BackgroundColor <- new UIColor(nfloat(color.red), nfloat(color.green), nfloat(color.blue), nfloat(color.alpha))
        |> writer --
            fun this (AlignItems align) ->
                this.css.AlignItems <- 
                    match align with
                    | Align.Auto -> CSSAlign.Auto
                    | Align.Start -> CSSAlign.FlexStart
                    | Align.Center -> CSSAlign.Center
                    | Align.End -> CSSAlign.FlexEnd
                    | Align.Stretch -> CSSAlign.Stretch
        |> writer --
            fun this (JustifyContent justify) ->
                this.css.JustifyContent <-
                    match justify with
                    | Justify.Start -> CSSJustify.FlexStart
                    | Justify.Center -> CSSJustify.Center
                    | Justify.End -> CSSJustify.FlexEnd
                    | Justify.SpaceBetween -> CSSJustify.SpaceBetween
                    | Justify.SpaceAround -> CSSJustify.SpaceAround

    let inline controlDisposer (v:#Control) = 
        v.view.RemoveFromSuperview()
        v.view.Dispose()

    let createControl view = 
        Control<_>(view, CSSNode())

    let createView identity props = 
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

        createAncestorResource viewAccessor controlDisposer nestingAdapter identity view props
        
    let createButton identity props = 
        let button = UIButton.FromType(UIButtonType.System)
        let control = createControl button
        createResource buttonAccessor controlDisposer identity control props

    let createLabel identity props =
        let label = new UILabel()
        let control = createControl label
        createResource labelAccessor controlDisposer identity control props

    let registerResources() =
        Registry.register "Button" createButton
        Registry.register "Text" createLabel
        Registry.register "View" createView

    let private dispatchEventAndUpdate (root: MountedRoot) (comp : Component) event = 
        comp.dispatchEvent event
        root.update()
        
    let private registerEventRoot mountedRoot = 
        registerEventRoot (dispatchEventAndUpdate mountedRoot)

    let debug = System.Diagnostics.Debug.WriteLine

    type UIRootView() = 
        inherit UIView()

        let mutable ourView : UIView = null

        member this.clearView() = 
            ourView.RemoveFromSuperview()
            ourView <- null

        member this.setView(view)  = 
            assert(ourView = null)
            ourView <- view
            this.Add(view)

        override this.LayoutSubviews() = 
            let frame = CGRect(nfloat(0.0), nfloat(0.0), nfloat(this.Frame.Width |> float), nfloat(this.Frame.Height |> float))
            ourView.Frame <- frame


    let renderAsView element = 
        let view = new UIRootView()

        let mountView (view : UIRootView) index (control:Control) = 
            view.setView(control.view)

        let unmountView (view : UIRootView) (control:Control) = 
            view.clearView()

        let nestingAdapter = NestingAdapter(mountView, unmountView)
        let disposer _ = ()

        let controllerResource = 
            Resources.createAncestorResource 
                PropertyAccessor.accessor
                disposer 
                nestingAdapter
                "rootView"
                view
                []

        let controllerState = { name="UIRootView"; resource = controllerResource }

        let mounted = UI.mountRoot controllerState element
        registerEventRoot mounted |> ignore
        view :> UIView

    let renderToController element (target : UIViewController) =
        if target.IsViewLoaded then
            failwith "FsReact can only render to an UIViewController that has no view yet."

        target.View <- renderAsView element

(*
    let renderToView element (target : UIView) = 

        let mountView (view: UIView) index (control: Control) = 
            assert(index = 0)
            view.Add(control.view)
            control.view.Frame = 
            control.view.SetNeedsLayout()

        let unmountView (view: UIView) (control: Control) =
            control.view.RemoveFromSuperview()

        let nestingAdapter = NestingAdapter(mountView, unmountView)
        let disposer _ = ()

        let resource = 
            Resources.createAncestorResource 
                PropertyWriter.empty 
                disposer 
                nestingAdapter
                target 
                []

        let state = { name="UIView"; resource = resource }

        let mounted = UI.mountRoot state element
        registerEventRoot mounted |> ignore
*)