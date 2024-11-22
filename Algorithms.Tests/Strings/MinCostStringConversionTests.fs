namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type MinCostStringConversionTests () =

    [<TestMethod>]
    [<DataRow("abbbaba", "abbba")>]
    [<DataRow("ababa", "ababa")>]
    member this.assembleTransformation (ops:string list, i:int, j:int, expected:string list) =
        let actual = MinCostStringConversion.assembleTransformation(ops, i, j)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow("abbbaba", "abbba")>]
    [<DataRow("ababa", "ababa")>]
    member this.assembleTransformation (sourceString:string, destinationString:string, copyCost:int, replaceCost:int, deleteCost:int, insertCost:int, expected:int list * string list) =
        let actual = MinCostStringConversion.computeTransformTables(sourceString,destinationString,copyCost,replaceCost,deleteCost,insertCost)
        Assert.AreEqual(expected, actual)

