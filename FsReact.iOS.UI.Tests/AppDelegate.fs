namespace FsReact.iOS.UI.Tests

open UIKit
open Foundation

open FsReact
open FsReact.iOS
open FsReact.Core

open ComponentTests
open PopoverTests

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    // window must be created in FinishedLaunching, otherwise MainScreen.Bounds are wrong, which results
    // in a rotation problem, when we start in landscape mode.

    let mutable _window = null

    override this.FinishedLaunching (app, options) =
        Tracer.set (fun (str:string) -> System.Console.WriteLine str)
    
        // can't make static initialization of F# modules work.       
        registerResources()


        let currentTest = counter
        let currentTest = replaceRoot
        let currentTest = replaceNested
        let currentTest = twoComponents
        let currentTest = nestedViewWithLayoutChange
        let currentTest = popoverTest
        
        let createView() = renderAsView (element currentTest [])
        
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

