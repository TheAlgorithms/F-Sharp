namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings



[<TestClass>]
type NaiveStringSearchTests () =

    [<TestMethod>]
    member this.naivePatternSearch () =
        let actual = NaiveStringSearch.naivePatternSearch("ABAAABCDBBABCDDEBCABC", "ABC")
        let expected = [4; 10; 18]
        Assert.AreEqual(expected, actual)

