namespace FsReact

open FsReact
open FsReact.UI
open UIKit
open Facebook.CSSLayout
open Resources
open System

module iOS =

    let buttonWriter = 
        PropertyWriter.empty<UIButton>
        |. fun b -> Text ""
        |+ fun b (Text t) -> b.SetTitle(t, UIControlState.Normal)
        |+ fun b (OnClick (c, e)) -> ()
        (*
            let eh = new EventHandler(fun x -> Core.scheduleEvent(c, e)) 
            b.TouchDown.AddHandler(eh)
            fun () ->
                b.TouchDown.RemoveHandler(eh)
        *)

    let labelWriter = 
        PropertyWriter.empty<UILabel>
        |+ fun l (Text t) -> l.Text <- t

    let viewWriter = 
        PropertyWriter.empty<UIView>

    let inline viewDisposer v = 
        let v = v :> UIView
        v.RemoveFromSuperview()
        v.Dispose()

    let createView = 
        let view = new UIView()
        createResource viewWriter viewDisposer view
        
    let createButton = 
        let button = UIButton.FromType(UIButtonType.System)
        createResource buttonWriter viewDisposer button

    let createLabel =
        let label = new UILabel()
        createResource labelWriter viewDisposer label

    let registerResources() =
        Registry.register "Button" createButton
        Registry.register "Text" createLabel
        Registry.register "View" createView

    let renderToController element (target : UIViewController) =
        if target.IsViewLoaded then
            failwith "React can only render to an UIViewController that has no view yet."

        let mounted = UI.mountRoot element
        let topPresenter = UI.resolveTopPresenter mounted
        target.View <- topPresenter.resource.instance :?> UIView

    let renderToView element (target : UIView) = 
        if (target.Subviews.Length <> 0) then
            failwith "React.render can only render to an UIView with no subviews"

        let mounted = UI.mountRoot element
        let topPresenter = UI.resolveTopPresenter mounted
        target.AddSubview(topPresenter.resource.instance :?> UIView)



