namespace Dialog.iOS.UI.Tests

open UIKit
open Foundation

open Dialog
open Dialog.UI.iOS
open Dialog.Core

open LayoutTests
open PopoverTests
open ComponentTests
open InteractionTests
open StandardControls


[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()


    // window must be created in FinishedLaunching, otherwise MainScreen.Bounds are wrong, which results
    // in a rotation problem, when we start in landscape mode.

    let mutable _window = null

    override this.FinishedLaunching (app, options) =
        Tracer.set (fun (str:string) -> System.Console.WriteLine str)
    
        let currentTest = counter
        let currentTest = replaceRoot
        let currentTest = replaceNested
        let currentTest = twoComponents
        let currentTest = nestedViewWithLayoutChange
        let currentTest = centeredItemsDoNotUseSpaceBetweenInRowDirection
        let currentTest = iconViewTest
        let currentTest = popoverTest
        let currentTest = dynamicContentTest
        let currentTest = rangeLimitedSlider
        let currentTest = standardControls
        
        let createView() = renderAsView (render currentTest [])
        
        _window <- new UIWindow (UIScreen.MainScreen.Bounds)

        let controller = new RootViewController(createView);
        _window.RootViewController <- controller
        _window.MakeKeyAndVisible ()

        true

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main (args, null, "AppDelegate")
        0

