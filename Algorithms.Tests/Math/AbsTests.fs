namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type AbsTests() =

    [<TestMethod>]
    [<DataRow(-10, 10)>]
    [<DataRow(-1, 1)>]
    [<DataRow(0, 0)>]
    [<DataRow(1, 1)>]
    [<DataRow(10, 10)>]
    member this.Test(input: int, expected: int) =
        let actual = Abs.absVal input
        Assert.AreEqual(expected, actual)
