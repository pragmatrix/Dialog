namespace FsReact

open FsReact
open FsReact.Core
open FsReact.UI
open UIKit
open Resources
open System

module iOS =

    open VDOM

    let buttonWriter = 
        PropertyWriter.empty<UIButton>
        |. (Text "", fun b (Text t) -> b.SetTitle(t, UIControlState.Normal))
        
        |+ fun b (OnClick e) -> 
            let handler = EventHandler(fun o ea -> dispatchEvent e)
            b.TouchDown.AddHandler handler
            fun () -> b.TouchDown.RemoveHandler handler

    let labelWriter = 
        PropertyWriter.empty<UILabel>
        |. (Text "", fun l (Text t) -> l.Text <- t)

    let viewWriter = 
        PropertyWriter.empty<UIView>

    let inline viewDisposer v = 
        let v = v :> UIView
        v.RemoveFromSuperview()
        v.Dispose()

    let createView props = 
        let view = new UIView()
        createResource viewWriter viewDisposer view props
        
    let createButton props = 
        let button = UIButton.FromType(UIButtonType.System)
        createResource buttonWriter viewDisposer button props

    let createLabel props =
        let label = new UILabel()
        createResource labelWriter viewDisposer label props

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
            failwith "React can only render to an UIViewController that has no view yet."

        let mounted = UI.mountRoot element
        registerEventRoot mounted |> ignore

        let topPresenter = UI.resolveTopPresenter mounted
        target.View <- topPresenter.resource.instance :?> UIView


    let renderToView element (target : UIView) = 
        if (target.Subviews.Length <> 0) then
            failwith "React.render can only render to an UIView with no subviews"

        let mounted = UI.mountRoot element
        registerEventRoot mounted |> ignore

        let topPresenter = UI.resolveTopPresenter mounted
        target.AddSubview(topPresenter.resource.instance :?> UIView)



