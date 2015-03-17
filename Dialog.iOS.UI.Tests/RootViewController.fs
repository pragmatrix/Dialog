namespace Dialog.iOS.UI.Tests

open System
open System.Drawing

open UIKit
open Foundation

[<Register ("RootViewController")>]
type RootViewController (viewLoader: unit -> UIView) =
    inherit UIViewController ()

    // Release any cached data, images, etc that aren't in use.
    override this.DidReceiveMemoryWarning () =
        base.DidReceiveMemoryWarning ()

    override this.LoadView() = 
        this.View <- viewLoader()

    // Perform any additional setup after loading the view, typically from a nib.
    override this.ViewDidLoad () =
        base.ViewDidLoad ()

    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation (orientation) =
        true

    override this.PrefersStatusBarHidden() = true
