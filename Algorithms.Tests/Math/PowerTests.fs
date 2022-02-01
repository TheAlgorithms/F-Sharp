namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type PowerTests() =

    [<TestMethod>]
    [<DataRow(0, 100, 0)>]
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
    member this.FoldFunction_Valid(num: int, pow: int, expected: int) =
        let actual = Power.byFoldFunction num pow
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow(-2, -2, 0)>]
    [<DataRow(-2, -1, 0)>]
    [<DataRow(-2, 0, 1)>]
    [<DataRow(-2, 1, -2)>]
    [<DataRow(-2, 2, 4)>]
    [<DataRow(-2, 3, -8)>]
    [<DataRow(-1, -3, -1)>]
    [<DataRow(-1, -2, 1)>]
    [<DataRow(-1, -1, -1)>]
    [<DataRow(-1, 0, 1)>]
    [<DataRow(-1, 1, -1)>]
    [<DataRow(-1, 2, 1)>]
    [<DataRow(-1, 3, -1)>]
    [<DataRow(0, 0, 1)>]
    [<DataRow(0, 1, 0)>]
    [<DataRow(0, 2, 0)>]
    [<DataRow(1, -2, 1)>]
    [<DataRow(1, -1, 1)>]
    [<DataRow(1, 0, 1)>]
    [<DataRow(1, 1, 1)>]
    [<DataRow(1, 2, 1)>]
    [<DataRow(2, 2, 4)>]
    [<DataRow(2, 3, 8)>]
    [<DataRow(2, 4, 16)>]
    [<DataRow(2, 8, 256)>]
    [<DataRow(2, 16, 65536)>]
    [<DataRow(3, 5, 243)>]
    [<DataRow(5, 3, 125)>]
    [<DataRow(10, 4, 10000)>]
    member this.ByRecursion_Valid(num: int, pow: int, expected: int) =
        let actual = Power.byRecursion num pow
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow(0, -1)>]
    [<DataRow(0, -2)>]
    member this.ByRecursion_Invalid(num: int, pow: int) =
        let action =
            new System.Action(fun () -> Power.byRecursion num pow |> ignore)

        Assert.ThrowsException<System.DivideByZeroException>(action)
        |> ignore
