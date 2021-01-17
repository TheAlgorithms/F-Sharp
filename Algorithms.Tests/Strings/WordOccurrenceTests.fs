namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type WordOccurrenceTests () =

    [<TestMethod>]
    member this.wordOccurrence () =
        let mutable expected : Map<string,int> = Map.empty
        expected <- expected.Add("Two",1)
        expected <- expected.Add("spaces",1)
        let actual = WordOccurrence.wordOccurrence("Two  spaces")
        Assert.AreEqual(expected, actual)

