module ComponentTests

open NUnit.Framework

open UIKit

[<TestFixture>]
type ComponentTests() = 
    [<Test>]
    member this.test() = 
        Assert.True(true)