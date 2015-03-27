namespace Dialog.UI

open System

open Dialog
open Dialog.UI
open Dialog.Services
open Dialog.Scanners
open Dialog.PropertyAccessor
open Dialog.Layout

open Facebook.CSSLayout

open CoreGraphics
open UIKit

module iOS =

    let private nf (v:float) = nfloat(v)

    type Convert = 
        static member rect (r: CGRect) = 
            { left = r.X |> float; top = r.Y |> float; width = r.Width |> float; height = r.Height |> float }
        static member rect (r: Rect) = 
            CGRect(nf r.left, nf r.top, nf r.width, nf r.height)

        static member toSize (width:float, height:float) =
            CGSize(nf width, nf height)

        static member size (size: CGSize) =
            (size.Width |> float, size.Height |> float)

        static member color color = 
            new UIColor(nfloat(color.red), nfloat(color.green), nfloat(color.blue), nfloat(color.alpha))

        static member color (color:UIColor) = 
            match color with
            | null -> Color.Transparent
            | _ ->
            let r, g, b, a = color.GetRGBA()
            { red = r |> float; green = g |> float; blue = b |> float; alpha = a |> float}

    type IOSView = IOSView of UIView
        
    let mkLayout (view: UIView) =
        let setFrame (r:Rect) = UIView.PerformWithoutAnimation(fun () -> view.Frame <- Convert.rect r)
        Layout(setFrame)
        
    type Control(view: UIView, layout: Layout) = 
        let _layout = layout

        member this.view = view
        member this.layout = _layout
        member this.useSizeThatFits() =
            let layoutFunction width = 
                this.view.SizeThatFits(CGSize(nf width, nf 0.0)) 
                |> Convert.size

            this.layout.setMeasure (SizeThatFits layoutFunction)

    type Control<'view when 'view :> UIView>(view: 'view, layout : Layout) = 
        inherit Control(view, layout)
        member this.view = view

    type UILayoutView() as this =
        inherit UIView()

        let _layout = mkLayout this

        member this.layout = _layout

        override this.LayoutSubviews() = 
            _layout.layoutNested(this.Bounds.Size |> Convert.size)
            
        member this.mountControl index (control: Control) =
            _layout.insertNested index control.layout
            this.InsertSubview(control.view, nint(index))

        member this.unmountControl (control: Control) = 
            control.view.RemoveFromSuperview()
            _layout.removeNested control.layout

    let mkHandler handler = EventHandler handler
    let mkEvent target msg reference = { message = msg; props = []; sender = reference }

    let controlProperties (get: 'a -> UIControl) accessor =
        accessor
        |> Lense.enter get
        |> reader -- fun this -> this.Enabled |> Activation.fromBoolean
        |> writer -- fun this (e:Switch) -> this.Enabled <- e.Boolean
        |> Lense.reset

    let fontProperties (get: 'a -> UIFont) (set: 'a -> UIFont -> unit) accessor = 
        accessor
        |> reader -- fun this -> (get this).PointSize |> float |> FontSize
        |> writer -- fun this (FontSize s) -> set this ((get this).WithSize(nfloat s))

    let controlAccessor = 
        accessorFor<Control>
        |> Layout.properties -- fun this -> this.layout :> CSSNode

        |> Lense.enter -- fun this -> this.view

        |> reader -- fun this -> this.Frame |> Convert.rect |> Frame
        |> reader -- fun this -> IOSView this

        |> reader -- fun this -> this.BackgroundColor |> Convert.color |> BackgroundColor
        |> writer -- fun this (BackgroundColor color) -> this.BackgroundColor <- Convert.color color
                
        |> materialize

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

    let createControl view = Control<_>(view, mkLayout view)
    
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
            let view = new UILayoutView()
            Control<_>(view, view.layout)

        let viewAccessor = 
            accessorFor<Control<UILayoutView>>
            |> extend controlAccessor
            |> Layout.viewProperties (fun this -> this.layout :> CSSNode)
            |> materialize

        let mounter (this : Control<UILayoutView>) (index:int) (nested : Control) = 
            this.view.mountControl index nested

        let unmounter (this : Control<UILayoutView>) (nested : Control) = 
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
            accessorFor<Control<UIButton>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.view :> UIControl
            |> fontProperties (fun c -> c.view.Font) (fun c f -> c.view.Font <- f)
            |> reader -- fun this -> this.view.Title(UIControlState.Normal) |> Text
            |> writer --
                fun this (Text t) ->
                    UIView.PerformWithoutAnimation (fun _ -> this.view.SetTitle(t, UIControlState.Normal))
                    this.layout.MarkDirty()

            |> mounter --
                fun this (Image source) ->
                    this.view.SetImage(loadImage source, UIControlState.Normal)
                    fun () -> 
                        this.view.SetImage(null, UIControlState.Normal)

            |> eventMounter -- fun this (OnClick e) -> e, this.view.TouchUpInside
            |> materialize

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
            accessorFor<Control<UISwitch>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.view :> UIControl
            |> reader -- fun this -> Switch.fromBoolean this.view.On
            |> writer -- fun this (sw:Switch) -> this.view.On <- sw.Boolean
            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged
            |> materialize

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
            control.layout.setMeasure (MinimumSize sliderDefaultSize)
            control

        let accessor = 
            accessorFor<Control<UISlider>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.view :> UIControl
            |> reader -- fun this -> SliderValue (this.view.Value |> float)
            |> writer -- fun this (SliderValue v) -> this.view.Value <- v |> float32
            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged
            |> materialize

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)

    let labelService =

        let labelAccessor = 
            accessorFor<Control<UILabel>>
            |> extend controlAccessor
            |> fontProperties (fun this -> this.view.Font) (fun this f -> this.view.Font <- f)
            |> reader -- fun this -> Text this.view.Text
            |> writer -- fun this (Text t) -> 
                this.view.Text <- t
                this.layout.MarkDirty()
            |> reader -- fun this -> this.view.TextColor |> Convert.color |> TextColor
            |> writer -- fun this (TextColor c) -> this.view.TextColor <- Convert.color c
            |> materialize

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
            accessorFor<Control<UIStepper>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.view :> UIControl

            |> reader -- fun this -> this.view.MaximumValue |> Math.Round |> int |> Steps
            |> writer -- fun this (Steps steps) -> this.view.MaximumValue <- steps |> float

            |> reader -- fun this -> this.view.Value |> Math.Round |> int |> StepperValue
            |> writer -- fun this (StepperValue v) -> this.view.Value <- v |> float

            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged
            |> materialize

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)

    let segmentedService = 
        let construct() = 
            let segmented = new UISegmentedControl()
            let control = createControl segmented
            control.useSizeThatFits()
            control

        let accessor = 
            accessorFor<Control<UISegmentedControl>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.view :> UIControl

            |> reader -- fun this ->
                match int this.view.SelectedSegment with
                | -1 -> None |> SelectedSegment
                | v -> v |> Some |> SelectedSegment

            |> writer -- fun this (SelectedSegment selected) ->
                let v = 
                    match selected with
                    | None -> -1
                    | Some v -> v

                this.view.SelectedSegment <- nint v

            |> reader -- fun this ->
                let titles = [
                    for i in 0..(int this.view.NumberOfSegments)-1 do
                        yield Text <| this.view.TitleAt (nint i)
                   ]
                titles |> Segments

            |> writer -- fun this (Segments segments) ->
                let segments = segments |> List.toArray
                
                for i in 0..segments.Length-1 do
                    let title = segments.[i] |> function Text t -> t
                    if i < int this.view.NumberOfSegments then
                        this.view.SetTitle(title, nint i)
                    else 
                        this.view.InsertSegment(title, nint i, true)

                for i in segments.Length..(int this.view.NumberOfSegments-1) do
                    this.view.RemoveSegmentAtIndex(nint i, true)

            |> eventMounter -- fun this (OnChanged e) -> e, this.view.ValueChanged
            |> materialize

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)
    
    let entryService = 
        let construct() = 
            let view = new UITextField()
            view.BorderStyle <- UITextBorderStyle.RoundedRect
            view.ClearButtonMode <- UITextFieldViewMode.WhileEditing
            view.Font <- UIFont.SystemFontOfSize(nfloat 14.)
            let control = createControl view
            // IB default
            control.useSizeThatFits(); //(97., 30.)
            control

        let accessor =
            accessorFor<Control<UITextField>>
            |> extend controlAccessor
            |> controlProperties (fun c -> c.view :> UIControl)

            |> reader -- fun this -> this.view.Text |> Text
            |> writer -- fun this (Text t) -> 
                this.view.Text <- t
                this.layout.MarkDirty()

            |> mounter -- 
                fun this Secure -> 
                    this.view.SecureTextEntry <- true
                    fun () -> this.view.SecureTextEntry <- false

            |> eventMounter -- fun this (OnCompleted e) -> e, this.view.Ended
            |> eventMounter -- fun this (OnChanged e) -> 
                this.layout.MarkDirty()
                e, this.view.ValueChanged
            |> materialize

        controlClassPrototype()
            .Constructor(construct)
            .PropertyAccessor(accessor)
        
    let imageService = 

        let accessor =
            accessorFor<Control<UIImageView>>
            |> extend controlAccessor
            |> mounter --
                fun this (source:Source) ->
                    this.view.Image <- loadImage source
                    fun () ->
                        this.view.Image <- null
            |> materialize

        let constructor'() =
            let image = new UIImageView()
            let control = createControl image
            control.useSizeThatFits()
            control

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(accessor)

    type ViewController() = 
        let _rootView = new UILayoutView()
        let _controller = new UIViewController()
        
        do
            _controller.View <- _rootView

        member this.updateLayout() =
            let preferred = _rootView.layout.calculatePreferredSize()
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
        let _rootView = new UILayoutView()
        let _contained = new UIViewController()
        let _controller = new UIPopoverController(_contained)
        do
            _contained.View <- _rootView

        member val anchor : Reference option = None with get, set

        member this.updateLayout() =
            let preferred = _rootView.layout.calculatePreferredSize()
            _contained.PreferredContentSize <- Convert.toSize preferred

        interface MountingNotifications with
            member this.mounted() = 
                match this.anchor with
                | None -> ()
                | Some ref ->
                let refFrame = ref.get (function Frame f -> f)
                let refView = ref.get (function IOSView v -> v)

                let preferred = _rootView.layout.calculatePreferredSize()
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
            |> materialize

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

    UI.labelService.register labelService.Instantiate
    UI.imageService.register imageService.Instantiate

    UI.buttonService.register buttonService.Instantiate
    UI.switchService.register switchService.Instantiate
    UI.sliderService.register sliderService.Instantiate
    UI.stepperService.register stepperService.Instantiate
    UI.segmentedService.register segmentedService.Instantiate

    UI.entryService.register entryService.Instantiate

    UI.viewService.register viewService.Instantiate
    UI.popoverService.register popoverService.Instantiate
