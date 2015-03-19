namespace Dialog

open Dialog
open Dialog.UI
open Dialog.Services
open Dialog.Scanners
open Dialog.PropertyAccessor

open CoreGraphics
open UIKit

open Facebook.CSSLayout

open System
open System.Collections.Generic

module iOS =

    let private nf (v:float) = nfloat(v)

    type Convert = 
        static member rect (r: CGRect) = 
            { left = r.X |> float; top = r.Y |> float; width = r.Width |> float; height = r.Height |> float }
        static member rect (r: Rect) = 
            CGRect(nf r.left, nf r.top, nf r.width, nf r.height)

        static member toSize (width:float, height:float) =
            CGSize(nf width, nf height)

        static member color color = 
            new UIColor(nfloat(color.red), nfloat(color.green), nfloat(color.blue), nfloat(color.alpha))
        static member color (color:UIColor) = 
            match color with
            | null -> Color.Transparent
            | _ ->
            let r, g, b, a = color.GetRGBA()
            { red = r |> float; green = g |> float; blue = b |> float; alpha = a |> float}

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
            UIView.PerformWithoutAnimation(fun () -> view.Frame <- frame);
            css.MarkLayoutSeen()

        member this.useSizeThatFits() =
            this.css.MeasureFunction <-
                fun node width ->
                    let sz = this.view.SizeThatFits(new CGSize(nfloat width, nfloat 0.0))
                    MeasureOutput(sz.Width |> float32, sz.Height |> float32)

        member this.minimumSize(width, height) = 
            this.css.MeasureFunction <-
                fun _ _ ->
                    MeasureOutput(width |> float32, height |> float32)

    type Control<'view when 'view :> UIView>(view: 'view, css: CSSNode) = 
        inherit Control(view, css)
        member this.view = view


    type UICSSLayoutView(css: CSSNode) =
        inherit UIView()

        let controls = HashSet<Control>()

        let mutable _preferredSize = (0., 0.)

        member this.calculatePreferredSize() = 

            css.StyleWidth <- CSSConstants.Undefined
            css.StyleHeight <- CSSConstants.Undefined

            if (css.IsDirty) then
                css.CalculateLayout()
                if css.HasNewLayout then
                    _preferredSize <- css.LayoutWidth |> float, css.LayoutHeight |> float
                    css.MarkLayoutSeen()
            
            _preferredSize

        override this.LayoutSubviews() = 

            if (css.Parent <> null) then ()
            else
            let bounds = this.Bounds
            css.StyleWidth <- bounds.Width |> float32
            css.StyleHeight <- bounds.Height |> float32

            if (not css.IsDirty) then 
                ()
            else

            css.CalculateLayout()
            if (not css.HasNewLayout) then ()
            else
            this.updateCSS()
            css.MarkLayoutSeen()

        member this.updateCSS() =
            controls |> Seq.iter (fun ctrl -> ctrl.updateCSS())
            
        member this.mountControl index (control: Control) =
            css.InsertChild(index, control.css)
            this.InsertSubview(control.view, nint(index))
            controls.Add control |> ignore

        member this.unmountControl (control: Control) = 
            controls.Remove control |> ignore
            control.view.RemoveFromSuperview()
            control.css.RemoveSelf()            

    type View(view: UICSSLayoutView, css:CSSNode) = 
        inherit Control<UICSSLayoutView>(view, css)
        override this.updateCSS() =
            base.updateCSS()
            view.updateCSS()

    let mkHandler handler = EventHandler handler
    let mkEvent target msg reference = { message = msg; props = []; sender = reference }

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

    let controlProperties (get: 'a -> UIControl) accessor =
        accessor
        |> reader -- fun this -> (get this).Enabled |> Activation.fromBoolean
        |> writer -- fun this (e:Switch) -> (get this).Enabled <- e.Boolean

    let fontProperties (get: 'a -> UIFont) (set: 'a -> UIFont -> unit) accessor = 
        accessor
        |> reader -- fun this -> (get this).PointSize |> float |> FontSize
        |> writer -- fun this (FontSize s) -> set this ((get this).WithSize(nfloat s))


    let controlAccessor = 
        accessorFor<Control>
        |> reader -- fun this -> this.view.Frame |> Convert.rect |> Frame
        |> reader -- fun this -> IOSView this.view

        |> reader -- fun this -> this.view.BackgroundColor |> Convert.color |> BackgroundColor
        |> writer -- fun this (BackgroundColor color) -> this.view.BackgroundColor <- Convert.color color
                
        |> reader -- fun this -> this.css.AlignSelf |> Convert.align |> AlignSelf
        |> writer -- fun this (AlignSelf align) -> this.css.AlignSelf <- Convert.align align

        |> reader -- fun this -> this.css.Flex |> float |> Flex
        |> writer -- fun this (Flex f) -> this.css.Flex <- f |> float32

        |> reader -- fun this -> getSpacing this.css.GetMargin |> Margin
        |> writer -- fun this (Margin spacing) -> setSpacing spacing this.css.SetMargin

        |> reader -- fun this -> getSpacing this.css.GetBorder |> Border
        |> writer -- fun this (Border spacing) -> setSpacing spacing this.css.SetBorder

        |> reader -- fun this -> getSpacing this.css.GetPadding |> Padding
        |> writer -- fun this (Padding spacing) -> setSpacing spacing this.css.SetPadding

        |> reader -- fun this -> this.css.StyleWidth |> float |> Width
        |> writer -- fun this (Width width) -> this.css.StyleWidth <- width |> float32

        |> reader -- fun this -> this.css.StyleHeight |> float |> Height
        |> writer -- fun this (Height height) -> this.css.StyleHeight <- height |> float32

    let eventMounter f accessor = 
        accessor
        |> eventMounter --
            fun this accessor e ->
                let (comp, msg), (e:IEvent<_, _>) = f this e
                let handler = mkHandler -- fun _ _ -> Events.dispatchEvent comp -- mkEvent this msg accessor
                e.AddHandler handler
                fun () -> e.RemoveHandler handler

    let inline controlDisposer (v:#Control) = 
        v.view.RemoveFromSuperview()
        v.view.Dispose()

    let createControl view = 
        Control<_>(view, CSSNode())
    
    let controlClassPrototype() = 
        Define.Service()
            .Destructor(controlDisposer)
    
    let nestedControlScanner = 

        let isControlType = (=) controlType

        let typeTester t = 
            match isControlType t with
            | true -> Include
            | false -> Skip

        recursiveNativeTypeScanner typeTester

    let viewService = 
        
        let constructor'() = 
            let css = CSSNode()
            let view = new UICSSLayoutView(css)
            View(view, css)

        let viewAccessor = 
            accessorFor<View>.extend controlAccessor

            |> reader -- 
                fun this -> 
                    match this.css.FlexDirection with
                    | CSSFlexDirection.Column -> LayoutDirection.Column
                    | CSSFlexDirection.Row -> LayoutDirection.Row
                    | unexpected -> failwithf "unexpected FlexDirection: %A" unexpected
            |> writer --
                fun this (direction : LayoutDirection) ->
                    this.css.FlexDirection <-
                    match direction with
                    | LayoutDirection.Column -> CSSFlexDirection.Column
                    | LayoutDirection.Row -> CSSFlexDirection.Row

            |> reader --
                fun this ->
                    match this.css.JustifyContent with
                    | CSSJustify.FlexStart -> JustifyContent.Start
                    | CSSJustify.Center -> JustifyContent.Center
                    | CSSJustify.FlexEnd -> JustifyContent.End
                    | CSSJustify.SpaceBetween -> JustifyContent.SpaceBetween
                    | CSSJustify.SpaceAround -> JustifyContent.SpaceAround
                    | unexpected -> failwithf "unexpected JustifyContent: %A" unexpected
            |> writer --
                fun this (justify : JustifyContent) ->
                    this.css.JustifyContent <-
                    match justify with
                    | JustifyContent.Start -> CSSJustify.FlexStart
                    | JustifyContent.Center -> CSSJustify.Center
                    | JustifyContent.End -> CSSJustify.FlexEnd
                    | JustifyContent.SpaceBetween -> CSSJustify.SpaceBetween
                    | JustifyContent.SpaceAround -> CSSJustify.SpaceAround

            |> reader -- fun this -> this.css.AlignItems |> Convert.align |> AlignItems
            |> writer -- fun this (AlignItems align) -> this.css.AlignItems <- Convert.align align


            |> reader --
                fun this ->
                    match this.css.Wrap with
                    | CSSWrap.NoWrap -> Wrap.NoWrap
                    | CSSWrap.Wrap -> Wrap.Wrap
                    | unexpected -> failwithf "unexpected Wrap: %A" unexpected
            |> writer --
                fun this (wrap : Wrap) ->
                    this.css.Wrap <- 
                    match wrap with
                    | Wrap.NoWrap -> CSSWrap.NoWrap
                    | Wrap.Wrap -> CSSWrap.Wrap


        let mounter (this : View) (index:int) (nested : Control) = 
            this.view.mountControl index nested

        let unmounter (this : View) (nested : Control) = 
            this.view.unmountControl nested

        controlClassPrototype()
            .Constructor(constructor')
            .Scanner(nestedControlScanner)
            .NestingAdapter(mounter, unmounter)
            .PropertyAccessor(viewAccessor)
            .Destructor(controlDisposer)
        
    let private loadImage source =
        match source with
        | Resource name -> 
            let img = UIImage.FromBundle(name) 
            match img with
            | null -> failwithf "failed to load image %A from bundle" source
            | _ -> img

    let buttonService = 

        let constructor'() = 
            let button = UIButton.FromType(UIButtonType.System)
            let control = createControl button
            control.useSizeThatFits()
            control

        let buttonAccessor = 
            accessorFor<Control<UIButton>>.extend controlAccessor
            |> controlProperties (fun c -> c.view :> UIControl)
            |> fontProperties (fun c -> c.view.Font) (fun c f -> c.view.Font <- f)
            |> reader -- fun this -> this.view.Title(UIControlState.Normal) |> Text
            |> writer --
                fun this (Text t) ->
                    UIView.PerformWithoutAnimation (fun _ -> this.view.SetTitle(t, UIControlState.Normal))
                    this.css.MarkDirty()

            |> mounter --
                fun this (Image source) ->
                    this.view.SetImage(loadImage source, UIControlState.Normal)
                    fun () -> 
                        this.view.SetImage(null, UIControlState.Normal)

            |> eventMounter -- fun this (OnClick e) -> e, this.view.TouchUpInside

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(buttonAccessor)

    let switchService = 
        let construct() =
            let switch = new UISwitch()
            let control = createControl switch
            control.useSizeThatFits()
            control

        let accessor = 
            accessorFor<Control<UISwitch>>.extend controlAccessor
            |> controlProperties (fun c -> c.view :> UIControl)
            |> reader -- fun this -> Switch.fromBoolean this.view.On
            |> writer -- fun this (sw:Switch) -> this.view.On <- sw.Boolean
            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)

    let sliderDefaultSize = 
        use sw = new UISlider()
        sw.Frame.Width |> float, sw.Frame.Height |> float

    let sliderService = 
        let construct() = 
            let slider = new UISlider()
            let control = createControl slider
            control.minimumSize(sliderDefaultSize)
            control

        let accessor = 
            accessorFor<Control<UISlider>>.extend controlAccessor
            |> controlProperties (fun c -> c.view :> UIControl)
            |> reader -- fun this -> SliderValue (this.view.Value |> float)
            |> writer -- fun this (SliderValue v) -> this.view.Value <- v |> float32
            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)

    let labelService =

        let labelAccessor = 
            accessorFor<Control<UILabel>>.extend controlAccessor
            |> fontProperties (fun this -> this.view.Font) (fun this f -> this.view.Font <- f)
            |> reader -- fun this -> Text this.view.Text
            |> writer -- fun this (Text t) -> 
                this.view.Text <- t
                this.css.MarkDirty()
            |> reader -- fun this -> this.view.TextColor |> Convert.color |> TextColor
            |> writer -- fun this (TextColor c) -> this.view.TextColor <- Convert.color c

        let constructor'() = 
            let label = new UILabel()
            let control = createControl label
            control.useSizeThatFits()
            control

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(labelAccessor)


    let stepperService = 
        let construct() = 
            let stepper = new UIStepper()
            let control = createControl stepper
            control.useSizeThatFits()
            control

        let accessor = 
            accessorFor<Control<UIStepper>>.extend controlAccessor
            |> controlProperties (fun c -> c.view :> UIControl)

            |> reader -- fun this -> this.view.MaximumValue |> Math.Round |> int |> Steps
            |> writer -- fun this (Steps steps) -> this.view.MaximumValue <- steps |> float

            |> reader -- fun this -> this.view.Value |> Math.Round |> int |> StepperValue
            |> writer -- fun this (StepperValue v) -> this.view.Value <- v |> float

            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)

        
    let imageService = 

        let accessor =
            accessorFor<Control<UIImageView>>.extend controlAccessor
            |> mounter --
                fun this (source:Source) ->
                    this.view.Image <- loadImage source
                    fun () ->
                        this.view.Image <- null

        let constructor'() =
            let image = new UIImageView()
            let control = createControl image
            control.useSizeThatFits()
            control

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(accessor)

    type ViewController() = 
        let _css = new CSSNode()
        let _rootView = new UICSSLayoutView(_css)
        let _controller = new UIViewController()
        
        do
            _controller.View <- _rootView

        member this.updateLayout() =
            let preferred = _rootView.calculatePreferredSize()
            _controller.PreferredContentSize <- Convert.toSize preferred

        with
        member this.rootView = _rootView
        member this.controller = _controller

    let controllerService =
    
        let constructor'() = new ViewController()

        let mounter (this: ViewController) index (nested: Control) =
            this.rootView.mountControl index (nested)
        let unmounter (this: ViewController) (nested: Control) =
            this.rootView.unmountControl (nested)

        Define.Service()
            .Constructor(constructor')
            .NestingAdapter(mounter, unmounter)
            .Scanner(nestedControlScanner)
            .UpdateNotifier(fun p -> p.updateLayout())

    type Popover() = 
        let _css = new CSSNode()
        let _rootView = new UICSSLayoutView(_css)
        let _contained = new UIViewController()
        let _controller = new UIPopoverController(_contained)
        do
            _contained.View <- _rootView

        member val anchor : Reference option = None with get, set

        member this.updateLayout() =
            let preferred = _rootView.calculatePreferredSize()
            _contained.PreferredContentSize <- Convert.toSize preferred

        interface MountingNotifications with
            member this.mounted() = 
                match this.anchor with
                | None -> ()
                | Some ref ->
                let refFrame = ref.get (function Frame f -> f)
                let refView = ref.get (function IOSView v -> v)

                let preferred = _rootView.calculatePreferredSize()
                _contained.PreferredContentSize <- Convert.toSize preferred
                let localFrame = { left = 0.; top = 0.; width = refFrame.width; height = refFrame.height }
            
                _controller.PresentFromRect (Convert.rect localFrame, refView, UIPopoverArrowDirection.Any, true)

            member this.unmounting() =
                _controller.Dismiss(true)

        with
        member this.rootView = _rootView
        member this.containedController = _contained
        member this.controller = _controller
                    
    let popoverService =

        let popoverAccessor = 
            accessorFor<Popover>
            |> reader -- fun this -> this.containedController.Title |> Title
            |> writer -- fun this (Title t) -> this.containedController.Title <- t
            |> mounter --
                // this is probably a common pattern to abstract the lifetime of a property int
                // an option type.
                // also a property-mirror could be interesting, what about using attributes to specify properties?
                fun this (Anchor a) -> 
                    this.anchor <- Some a
                    fun () -> 
                    this.anchor <- None
            |> eventMounter -- fun this (OnDismissed d) -> d, this.controller.DidDismiss

        let constructor'() = 
            new Popover()

        let mounter (this: Popover) index (nested: Control) =
            this.rootView.mountControl index (nested)
        let unmounter (this: Popover) (nested: Control) =
            this.rootView.unmountControl (nested)

        Define.Service()
            .Constructor(constructor')
            .PropertyAccessor(popoverAccessor)
            .NestingAdapter(mounter, unmounter)
            .Scanner(nestedControlScanner)
            .UpdateNotifier(fun p -> p.updateLayout())

    let private dispatchEventAndUpdate (root: MountedRoot) (comp : Component) event = 
        comp.dispatchEvent event
        root.update()
        
    let private registerEventRoot mountedRoot = 
        Events.registerEventRoot (dispatchEventAndUpdate mountedRoot)

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

        member this.updateLayout() = 
            assert(ourView <> null)
            ourView.SetNeedsLayout()

        override this.LayoutSubviews() = 
            let frame = CGRect(nfloat(0.0), nfloat(0.0), nfloat(this.Frame.Width |> float), nfloat(this.Frame.Height |> float))
            ourView.Frame <- frame

    let renderAsView element = 
        let constructor'() = new UIRootView()

        let mountView (view : UIRootView) index (control:Control) = 
            view.setView(control.view)

        let unmountView (view : UIRootView) (control:Control) = 
            view.clearView()

        let viewService = 
            Define.Service()
                .Constructor(constructor')
                .NestingAdapter(mountView, unmountView)
                .Scanner(nestedControlScanner)
                // we need to kick the layout here, in case LayoutSubviews() is not called 
                // on the nested view, which may happen, when structural changes made to views further down only.
                .UpdateNotifier(fun rv -> rv.updateLayout())
                .Instantiate ("rootView", "/") []

        let systemService = 
            Services.createSystemService [controllerType] ("system", "/") []

        let mounted = UI.mountRoot [viewService; systemService] element
        registerEventRoot mounted |> ignore
        viewService.instance :> UIView

    let renderAsViewController element = 

        let controllerService = 
            controllerService.Instantiate ("rootController", "/") []

        let systemService = 
            Services.createSystemService [controllerType] ("system", "/") []

        let mounted = UI.mountRoot [controllerService; systemService] element
        registerEventRoot mounted |> ignore
        controllerService.instance.controller

    UI.buttonService.register buttonService.Instantiate
    UI.switchService.register switchService.Instantiate
    UI.sliderService.register sliderService.Instantiate
    UI.stepperService.register stepperService.Instantiate

    UI.labelService.register labelService.Instantiate
    UI.imageService.register imageService.Instantiate

    UI.viewService.register viewService.Instantiate
    UI.popoverService.register popoverService.Instantiate
