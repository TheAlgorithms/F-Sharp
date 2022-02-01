namespace Algorithms.Tests.Search

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Search

[<TestClass>]
type BinarySearchTests() =
    let data0 =
        [| -2147483468
           -10
           -2
           -1
           0
           1
           2
           10
           20
           20
           2147483467 |]

    [<TestMethod>]
    [<DataRow(-2147483468, 0)>]
    [<DataRow(-10, 1)>]
    [<DataRow(-2, 2)>]
    [<DataRow(-1, 3)>]
    [<DataRow(0, 4)>]
    [<DataRow(1, 5)>]
    [<DataRow(2, 6)>]
    [<DataRow(10, 7)>]
    [<DataRow(2147483467, 10)>]
    member this.Data0_Exists(num: int, expected: int) =
        let actual = BinarySearch.findIndex num data0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow(20)>]
    member this.Data0_Duplicates(num: int) =
        let actual = BinarySearch.findIndex num data0
        Assert.IsTrue(actual = 8 || actual = 9)

    [<TestMethod>]
    [<DataRow(5, -1)>]
    member this.Data0_None(num: int, expected: int) =
        let actual = BinarySearch.findIndex num data0
        Assert.AreEqual(expected, actual)
