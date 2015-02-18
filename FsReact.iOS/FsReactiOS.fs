module FsReactiOS

open FsReact
open FsReactUI
open UIKit
open Facebook.CSSLayout
open Resources
open System

module iOS =

    let buttonWriter = 
        PropertyWriter.empty<UIButton>
        |+ fun b (Text t) -> b.SetTitle(t, UIControlState.Normal)

    let labelWriter = 
        PropertyWriter.empty<UILabel>
        |+ fun l (Text t) -> l.Text <- t

    let viewWriter = 
        PropertyWriter.empty<UIView>

    let inline viewDisposer v = 
        let v = v :> UIView
        v.RemoveFromSuperview()
        v.Dispose()

    let createView props = 
        let view = new UIView()
        viewWriter.write view props
        createResource view viewWriter viewDisposer

    let createButton props = 
        let button = UIButton.FromType(UIButtonType.System)
        buttonWriter.write button props
        createResource button buttonWriter viewDisposer

    let createLabel props =
        let label = new UILabel()
        labelWriter.write label props
        createResource label labelWriter viewDisposer

    Registry.register "Button" createButton
    Registry.register "Text" createLabel
    Registry.register "View" createView

let renderToController element (target : UIViewController) =
    if target.IsViewLoaded then
        failwith "React can only render to an UIViewController that has no view yet."

    let mounted = mountRoot element
    let topPresenter = resolveTopPresenter mounted
    target.View <- topPresenter.resource.instance :?> UIView

let renderToView element (target : UIView) = 
    if (target.Subviews.Length <> 0) then
        failwith "React.render can only render to an UIView with no subviews"

    let mounted = mountRoot element
    let topPresenter = resolveTopPresenter mounted
    target.AddSubview(topPresenter.resource.instance :?> UIView)



