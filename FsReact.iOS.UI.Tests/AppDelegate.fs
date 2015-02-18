namespace FsReact.iOS.UI.Tests

open System
open UIKit
open Foundation

open FsReact.iOS
open FsReact.Core
open FsReact.UI

open ComponentTests

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    let window = new UIWindow (UIScreen.MainScreen.Bounds)

    override this.FinishedLaunching (app, options) =

        // can't make static initialization of F# modules work.       
        registerResources()

        let controller = new RootViewController();
        renderToController (element counter []) controller

        window.RootViewController <- controller
        window.MakeKeyAndVisible ()

        true

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main (args, null, "AppDelegate")
        0

