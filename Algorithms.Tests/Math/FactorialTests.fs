namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type FactorialTests () =
    
    [<TestMethod>]
    [<DataRow(1, 1)>]
    [<DataRow(2, 2)>]
    [<DataRow(3, 6)>]
    [<DataRow(5, 120)>]
    [<DataRow(8, 40320)>]
    [<DataRow(10, 3628800)>]
    member this.FactorialOf (num: int, expected: int) =
        let actual = Factorial.CalculateFactorial num
        Assert.AreEqual(expected, actual)