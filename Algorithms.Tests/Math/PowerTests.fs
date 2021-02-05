namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type PowerTests () =
    
    [<TestMethod>]
    [<DataRow(2, 2, 4)>]
    [<DataRow(2, 3, 8)>]
    [<DataRow(2, 4, 16)>]
    [<DataRow(2, 8, 256)>]
    [<DataRow(2, 16, 65536)>]
    [<DataRow(3, 5, 243)>]
    [<DataRow(5, 3, 125)>]
    [<DataRow(10, 4, 10000)>]
    [<DataRow(1, 2, 1)>]
    [<DataRow(1, 50, 1)>]
    member this.PowerOf (num: int, pow: int, expected: int) =
        let actual = Power.Pow num pow
        Assert.AreEqual(expected, actual)