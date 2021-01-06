namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type RabinKarpTests () =

    [<TestMethod>]
    [<DataRow("ABABX", "ABABZABABYABABX")>]
    [<DataRow("AAAB", "ABAAAAAB")>]
    [<DataRow("abcdabcy","abcxabcdabxabcdabcdabcy")>]
    [<DataRow("Lü","Lüsai")>]
    member this.rabinKarp (pattern:string, text:string) =
        let actual = RabinKarp.rabinKarp(pattern, text)
        Assert.IsTrue(actual)

