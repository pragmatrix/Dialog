namespace FsReact.iOS.UI.Tests

open System
open System.Drawing

open UIKit
open Foundation

[<Register ("RootViewController")>]
type RootViewController () =
    inherit UIViewController ()

    // Release any cached data, images, etc that aren't in use.
    override this.DidReceiveMemoryWarning () =
        base.DidReceiveMemoryWarning ()

    // Perform any additional setup after loading the view, typically from a nib.
    override this.ViewDidLoad () =
        base.ViewDidLoad ()

    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation (orientation) =
        true
