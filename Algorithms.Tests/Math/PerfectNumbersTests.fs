namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type PerfectNumbersTests() =

    [<TestMethod>]
    [<DataRow(-1, false)>]
    [<DataRow(0, false)>]
    [<DataRow(1, false)>]
    [<DataRow(2, false)>]
    [<DataRow(3, false)>]
    [<DataRow(4, false)>]
    [<DataRow(5, false)>]
    [<DataRow(6, true)>]
    [<DataRow(7, false)>]
    [<DataRow(27, false)>]
    [<DataRow(28, true)>]
    [<DataRow(496, true)>]
    [<DataRow(8128, true)>]
    [<DataRow(33550336, true)>]
    [<DataRow(33550337, false)>]
    member this.Test(n: int, expected: bool) =
        let actual = PerfectNumbers.isPerfect n
        Assert.AreEqual(expected, actual)
