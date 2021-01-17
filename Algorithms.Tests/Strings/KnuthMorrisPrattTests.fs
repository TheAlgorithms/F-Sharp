namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type KnuthMorrisPrattTests () =

    [<TestMethod>]
    member this.getFailureArray () =
        let actual = KnuthMorrisPratt.getFailureArray("aabaabaaa")
        let expected = [0; 1; 0; 1; 2; 3; 4; 5; 2]
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow("abc1abc12", "alskfjaldsabc1abc1abc12k23adsfabcabc")>]
    [<DataRow("ABABX", "ABABZABABYABABX")>]
    [<DataRow("AAAB", "ABAAAAAB")>]
    [<DataRow("abcdabcy", "abcxabcdabxabcdabcdabcy")>]
    member this.kmp (pattern:string, text:string) =
        let actual = KnuthMorrisPratt.kmp(pattern, text)
        Assert.IsTrue(actual)

