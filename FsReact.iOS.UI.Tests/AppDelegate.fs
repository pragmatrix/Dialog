namespace FsReact.iOS.UI.Tests

open System
open UIKit
open Foundation

open FsReactiOS
open FsReact

open ComponentTests

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    let window = new UIWindow (UIScreen.MainScreen.Bounds)

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        // If you have defined a root view controller, set it here:

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

