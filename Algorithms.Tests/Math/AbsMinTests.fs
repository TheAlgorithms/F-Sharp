namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type AbsMinTests() =

    [<TestMethod>]
    member this.Test() =
        Assert.AreEqual(-1, AbsMin.absMin [ -1; 1; 2; -2; -3; 3 ])
        Assert.AreEqual(1, AbsMin.absMin [ 1; -1; 2; -2; 3; -3 ])
        Assert.AreEqual(0, AbsMin.absMin [ 0; -1; 0 ])
