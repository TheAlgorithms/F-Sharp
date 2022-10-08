namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type AverageTests() =

    [<TestMethod>]
    member this.Test() =
        Assert.AreEqual(None, Average.average List.empty<float>)
        Assert.AreEqual(Some 2.0, Average.average [1.0; 2.0; 3.0])
        Assert.AreEqual(Some 1.0, Average.average [1.0])


