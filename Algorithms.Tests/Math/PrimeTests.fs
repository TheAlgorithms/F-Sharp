namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type PrimeTests() =

    [<TestMethod>]
    [<DataRow(-3, false)>]
    [<DataRow(-2, false)>]
    [<DataRow(-1, false)>]
    [<DataRow(0, false)>]
    [<DataRow(1, false)>]
    [<DataRow(2, true)>]
    [<DataRow(49, false)>]
    [<DataRow(50, false)>]
    [<DataRow(998244353, true)>]
    [<DataRow(1000000007, true)>]
    member this.IsPrime(input: int, expected: bool) =
        let actual = Prime.isPrime input
        Assert.AreEqual(expected, actual)
