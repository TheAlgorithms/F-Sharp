namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type LevenshteinDistanceTests () =

    [<TestMethod>]
    [<DataRow("planet", "planetary", 3)>]
    [<DataRow("", "test", 4)>]
    [<DataRow("book", "back", 2)>]
    [<DataRow("book", "book", 0)>]
    [<DataRow("test", "", 4)>]
    [<DataRow("", "", 0)>]
    [<DataRow("orchestration", "container", 10)>]
    member this.levenshteinDistance (firstWord:string, secondWord:string, expected:int) =
        let actual = LevenshteinDistance.levenshteinDistance(firstWord, secondWord )
        Assert.AreEqual(expected, actual)

