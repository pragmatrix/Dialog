namespace Dialog.UI

open System

open System.Windows
open System.Windows.Media
open System.Windows.Controls

open Dialog
open Dialog.Services
open Dialog.Scanners
open Dialog.PropertyAccessor

open Dialog.Layout
open Facebook.CSSLayout


module WPF = 
  
    type Convert() =
        static member size (sz: Size) = sz.Width, sz.Height
        static member toSize (width, height) = Size(width, height)
        static member rect (left, top, width, height) = { left = left; top = top; width = width; height = height }
        static member rect (r: UI.Rect) = Rect(r.left, r.top, r.width, r.height)
        static member color (brush: Media.Brush) = 
            match brush with
            | null -> Color.Transparent
            | :? SolidColorBrush as sc -> sc.Color |> Convert.color
            | _ -> Color.Transparent
        static member toBrush (color: UI.Color) = 
            color |> Convert.color |> fun c -> SolidColorBrush(c)
        static member color (color: Media.Color) = 
            { red = color.ScR |> float; green = color.ScG |> float; blue = color.ScB |> float; alpha = color.ScA |> float }
        static member color (color: UI.Color) =
            Media.Color.FromScRgb(color.alpha |> float32, color.red |> float32, color.green |> float32, color.blue |> float32)
        static member obj2string (content: obj) =
            match content with
            | null -> ""
            | content -> content.ToString()


    type WPFElement = WPFElement of FrameworkElement
    
    let mkLayout (element : FrameworkElement) =
        let setFrame (r:UI.Rect) =
            Canvas.SetLeft(element, r.left)
            Canvas.SetTop(element, r.top)
            element.Width <- r.width
            element.Height <- r.height

        Layout(setFrame)

    type Control(element: FrameworkElement, layout: Layout) = 
        let _layout = layout

        member this.element = element
        member this.layout = _layout
        member self.useSizeThatFits() = 
            let layoutFunction width = 
                let width = if Double.IsNaN(width) then Double.PositiveInfinity else width
                self.element.Measure(Size(width, Double.PositiveInfinity))
                // note that DesiredSize includes padding, so we have to subtract it to
                // measure the content only.
                let widthWithPadding, heightWithPadding = self.element.DesiredSize |> Convert.size
                let paddingH = self.layout.GetPadding(SpacingType.Left) + self.layout.GetPadding(SpacingType.Right) |> float
                let paddingV = self.layout.GetPadding(SpacingType.Top) + self.layout.GetPadding(SpacingType.Bottom) |> float
                let paddingH = if Double.IsNaN(paddingH) then 0. else paddingH
                let paddingV = if Double.IsNaN(paddingV) then 0. else paddingV
                max (widthWithPadding - paddingH) 0., max (heightWithPadding - paddingV) 0.

            self.layout.setMeasure (SizeThatFits layoutFunction)

    type Control<'element when 'element :> FrameworkElement>(element: 'element, layout: Layout) =
        inherit Control(element, layout)
        member this.element = element

    type LayoutCanvas() as this = 
        inherit Canvas()

        let _layout = mkLayout this

        member this.layout = _layout

        override this.ArrangeOverride(size: Size) = 
            let r = base.ArrangeOverride(size)
            _layout.layoutNested(size.Width, size.Height)
            r

        member this.mountControl index (control: Control) =
            _layout.insertNested index control.layout
            this.Children.Insert (index, control.element)

        member this.unmountControl (control: Control) = 
            this.Children.Remove control.element
            _layout.removeNested control.layout

    let mkHandler handler = RoutedEventHandler handler
    let mkEvent target msg reference = { message = msg; props = []; sender = reference }

    
    let controlProperties (get: 'a -> FrameworkElement) accessor =
        accessor
        |> Lense.enter get
        |> reader -- fun this -> this.IsEnabled
        |> writer -- fun this (e:Switch) -> this.IsEnabled <- e.Boolean
        |> Lense.reset
    
    let fontProperties (get: 'a -> Windows.Controls.Control) accessor = 
        accessor
        |> reader -- fun this -> (get this).FontSize |> float |> FontSize
        |> writer -- fun this (FontSize s) -> (get this).FontSize <- s
        
    let controlAccessor = 
        accessorFor<Control>
        |> Layout.properties -- fun this -> this.layout :> CSSNode

        |> Lense.enter -- fun this -> this.element

        |> reader -- fun this -> 
            (Canvas.GetLeft(this), Canvas.GetTop(this), this.ActualWidth, this.ActualHeight) 
            |> Convert.rect 
            |> Dialog.UI.Frame
        |> reader -- fun this -> WPFElement this
        |> materialize

    let backgroundProperty (get: 'a -> Brush) (set: 'a -> Brush -> unit) accessor = 
        accessor
        |> reader -- fun this -> (get this) |> Convert.color |> BackgroundColor
        |> writer -- fun this (BackgroundColor color) -> set this (Convert.toBrush color)
                            
    let eventMounter f accessor = 
        accessor
        |> eventMounter --
            fun this accessor e ->
                let (comp, msg), (e:IEvent<_, _>) = f this e
                let handler = mkHandler -- fun _ _ -> Events.dispatchEvent comp -- mkEvent this msg accessor
                e.AddHandler handler
                fun () -> e.RemoveHandler handler

    let createControl view = Control<_>(view, mkLayout view)

    let viewService = 
        
        let constructor'() = 
            let canvas = new LayoutCanvas()
            Control<_>(canvas, canvas.layout)

        let viewAccessor = 
            accessorFor<Control<LayoutCanvas>>
            |> extend controlAccessor
            |> Layout.viewProperties (fun this -> this.layout :> CSSNode)
            |> backgroundProperty (fun this -> this.element.Background) (fun this bg -> this.element.Background <- bg)
            |> materialize

        let mounter (this : Control<LayoutCanvas>) (index:int) (nested : Control) = 
            this.element.mountControl index nested

        let unmounter (this : Control<LayoutCanvas>) (nested : Control) = 
            this.element.unmountControl nested

        Define.Service()
            .Constructor(constructor')
            .Scanner(nestedControlScanner)
            .NestingAdapter(mounter, unmounter)
            .PropertyAccessor(viewAccessor)

    let mkSource source = 
        match source with
        | Resource r -> Imaging.BitmapImage(new Uri("/" + r, UriKind.Relative));

    let loadImage source = 
        source
        |> mkSource
        |> fun source ->
            let img = Controls.Image()
            img.Source <- source
            img

    let controlClassPrototype() = Define.Service()

    let mkTextBlock str = 
        let tb = TextBlock();
        TextOptions.SetTextFormattingMode(tb, TextFormattingMode.Display)
        TextOptions.SetTextRenderingMode(tb, TextRenderingMode.Aliased)
        tb.Text <- str
        tb

    let extractText (o:obj) = 
        match o with
        | :? TextBlock as tb -> tb.Text
        | null -> ""
        | str -> str.ToString()

    let labelService =

        let labelAccessor = 
            accessorFor<Control<Label>>
            |> extend controlAccessor
            |> fontProperties (fun c -> c.element :> Controls.Control)
            |> reader -- fun this -> this.element.Content |> extractText |> Text
            |> writer -- fun this (Text t) -> 
                this.element.Content <- mkTextBlock t
                this.layout.MarkDirty()
            |> reader -- fun this -> this.element.Foreground |> Convert.color |> TextColor
            |> writer -- fun this (TextColor c) -> this.element.Foreground <- Convert.toBrush c
            |> materialize

        let constructor'() = 
            let label = Label()
            let control = createControl label
            control.useSizeThatFits()
            control

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(labelAccessor)

    let imageService = 

        let accessor =
            accessorFor<Control<Image>>
            |> extend controlAccessor
            |> mounter --
                fun this (source:Source) ->
                    this.element.Source <- mkSource source
                    fun () ->
                        this.element.Source <- null
            |> materialize

        let constructor'() =
            let image = Image()
            let control = createControl image
            control.useSizeThatFits()
            control

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(accessor)


    let buttonService = 

        let constructor'() = 
            let button = Button()
            let control = createControl button
            control.layout.SetPadding(SpacingType.Horizontal, 10.f);
            control.layout.SetPadding(SpacingType.Vertical, 5.f);
            control.useSizeThatFits()
            control

        let buttonAccessor = 
            accessorFor<Control<Button>>
            |> extend controlAccessor
            |> controlProperties -- fun c -> c.element :> FrameworkElement
            |> fontProperties (fun c -> c.element :> Controls.Control)
            |> reader -- fun this -> this.element.Content |> extractText |> Text
            |> writer --
                fun this (Text t) ->
                    this.element.Content <- mkTextBlock t
                    this.layout.MarkDirty()

            |> mounter --
                fun self (Image source) ->
                    self.element.Content <- loadImage source
                    fun () -> 
                        self.element.Content <- null

            |> mounter --
                fun self (UI.ContentSize (w, h)) ->
                    self.layout.setMeasure (ContentSize (w, h))
                    fun () ->
                        self.useSizeThatFits()

            |> eventMounter -- fun this (OnClick e) -> e, this.element.Click
            |> materialize

        controlClassPrototype()
            .Constructor(constructor')
            .PropertyAccessor(buttonAccessor)

    let private dispatchEventAndUpdate (root: MountedRoot) (comp : Component) event = 
        comp.dispatchEvent event
        root.update()
        
    let private registerEventRoot mountedRoot = 
        Events.registerEventRoot (dispatchEventAndUpdate mountedRoot)

    type RootElement() = 
        inherit ContentControl()

        let mutable _element: FrameworkElement = null

        member this.clearElement() = 
            this.Content <- null
            _element <- null

        member this.setElement(element) =
            _element <- element
            element.Width <- Double.NaN
            element.Height <- Double.NaN

            this.Content <- element

    // http://stackoverflow.com/questions/18619093/how-to-execute-module-do-block
    let private ensureModuleIsInitialized = do (); ()

    let renderAsElement element = 
        ensureModuleIsInitialized
        let constructor'() = RootElement()

        let mountView (element : RootElement) index (control:Control) = 
            element.setElement control.element

        let unmountView (element : RootElement) (control:Control) = 
            element.clearElement()

        let viewService = 
            Define.Service()
                .Constructor(constructor')
                .NestingAdapter(mountView, unmountView)
                .Scanner(nestedControlScanner)
                // we need to kick the layout here, in case LayoutSubviews() is not called 
                // on the nested view, which may happen, when structural changes made to views further down only.
                // .UpdateNotifier(fun rv -> rv.updateLayout())
                .Instantiate ("rootView", "/") []

        let systemService = 
            Services.createSystemService [controllerType] ("system", "/") []

        let mounted = UI.mountRoot [viewService; systemService] element
        registerEventRoot mounted |> ignore
        viewService.instance :> FrameworkElement

    UI.labelService.register labelService.Instantiate
    UI.imageService.register imageService.Instantiate

    UI.buttonService.register buttonService.Instantiate

    UI.viewService.register viewService.Instantiate
