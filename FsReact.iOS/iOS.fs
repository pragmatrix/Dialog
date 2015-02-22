namespace FsReact

open FsReact
open FsReact.Core
open FsReact.UI
open UIKit
open Resources
open System
open Facebook.CSSLayout

module iOS =

    open VDOM

    type Control(view: UIView, css: CSSNode) = 
        member this.view = view
        member this.css = css

    type Control<'view when 'view :> UIView>(view: 'view, css: CSSNode) = 
        inherit Control(view, css)
        member this.view = view

    type View(view: UIView, css:CSSNode) = 
        inherit Control(view, css)

    let buttonWriter = 
        PropertyWriter.empty<Control<UIButton>>
        |. (Text "", fun this (Text t) -> 
            this.view.SetTitle(t, UIControlState.Normal)
            this.view.SizeToFit()
            )
        
        |+ fun this (OnClick e) -> 
            let handler = EventHandler(fun o ea -> dispatchEvent e)
            this.view.TouchDown.AddHandler handler
            fun () -> this.view.TouchDown.RemoveHandler handler

    let labelWriter = 
        PropertyWriter.empty<Control<UILabel>>
        |. (Text "", fun this (Text t) -> this.view.Text <- t)

    let viewWriter = 
        PropertyWriter.empty<View>
        |+ fun _ (Elements _) -> id

    let inline controlDisposer (v:#Control) = 
        v.view.RemoveFromSuperview()
        v.view.Dispose()

    let createControl view = 
        Control<_>(view, CSSNode())

    let createView props = 
        let view = new UIView()
        let css = CSSNode()
        let view = View(view, css)
        let mounter (this : View) (index:int) (nested : Control) = 
            this.css.InsertChild(index, nested.css)
            this.view.InsertSubview(nested.view, nint(index))

        let unmounter (this : View) (nested : Control) = 
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