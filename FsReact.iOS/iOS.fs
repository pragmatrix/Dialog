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

    let private nf (v:float) = nfloat(v)

    type Convert = 
        static member rect (r: CGRect) = 
            { left = r.X |> float; top = r.Y |> float; width = r.Width |> float; height = r.Height |> float }
        static member rect (r: Rect) = 
            CGRect(nf r.left, nf r.top, nf r.width, nf r.height)

    open VDOM


    type IOSView = IOSView of UIView

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

    let controlReader = 
        readerFor<Control>
        |> reader --
            fun this -> IOSView this.view

    let buttonReader = 
        readerFor<Control<UIButton>>.extend controlReader
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

        createAncestorResource viewAccessor controlDisposer (mounter, unmounter) identity view props
        
    let createButton identity props = 
        let button = UIButton.FromType(UIButtonType.System)
        let control = createControl button
        createResource buttonAccessor controlDisposer identity control props

    let createLabel identity props =
        let label = new UILabel()
        let control = createControl label
        createResource labelAccessor controlDisposer identity control props

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

    type Popover(props: Properties) = 
        let _rootView = new UIRootView()
        let _contained = new UIViewController()
        let _controller = new UIPopoverController(_contained)
        
        do
            _contained.View <- _rootView
            let ref = props |> Props.getFromList (function Anchor ref -> ref)
            let refFrame = ref.get (function Frame f -> f)
            let refView = ref.get (function IOSView v -> v)

            let localFrame = { left = 0.; top = 0.; width = refFrame.width; height = refFrame.height }
            
            _controller.PresentFromRect (Convert.rect localFrame, refView, UIPopoverArrowDirection.Any, true)
        with
        member this.rootView = _rootView
        member this.containedController = _contained
        member this.dismiss() = _controller.Dismiss(true)

    let popoverWriter = 
        accessor<Popover>
        |> writer (fun this (Title t) -> this.containedController.Title <- t)

    let popoverAdapter = 
        let mounter (this: Popover) index (nested: Control) =
            this.rootView.setView(nested.view)
        let unmounter (this: Popover) (nested: Control) =
            this.rootView.clearView()
        mounter, unmounter

    let createPopover identity props =
    
        let popover = new Popover(props)

        Resources.createAncestorResource
            popoverWriter
            (fun (popover : Popover) -> popover.dismiss())
            popoverAdapter
            identity
            popover
            props

    let registerResources() =
        Registry.register "Button" createButton
        Registry.register "Text" createLabel
        Registry.register "View" createView
        Registry.register "Popover" createPopover

    let private dispatchEventAndUpdate (root: MountedRoot) (comp : Component) event = 
        comp.dispatchEvent event
        root.update()
        
    let private registerEventRoot mountedRoot = 
        registerEventRoot (dispatchEventAndUpdate mountedRoot)

    let debug = System.Diagnostics.Debug.WriteLine


    let renderAsView element = 
        let view = new UIRootView()

        let mountView (view : UIRootView) index (control:Control) = 
            view.setView(control.view)

        let unmountView (view : UIRootView) (control:Control) = 
            view.clearView()

        let nestingAdapter = mountView, unmountView
        let disposer _ = ()

        let viewResource = 
            Resources.createAncestorResource 
                PropertyAccessor.accessor
                disposer 
                nestingAdapter
                "rootView"
                view
                []

        let viewState = { name="UIRootView"; resource = viewResource }

        let mounted = UI.mountRoot viewState element
        registerEventRoot mounted |> ignore
        view :> UIView

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