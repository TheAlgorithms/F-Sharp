namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type AbsMaxTests() =

    [<TestMethod>]
    member this.Test() =
        Assert.AreEqual(-3, AbsMax.absMax [ -1; 1; 2; -2; -3; 3 ])
        Assert.AreEqual(3, AbsMax.absMax [ 1; -1; 2; -2; 3; -3 ])
        Assert.AreEqual(-1, AbsMax.absMax [ 0; -1; 0 ])
