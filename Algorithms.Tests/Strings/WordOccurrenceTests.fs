namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type WordOccurrenceTests () =

    [<TestMethod>]
    member this.wordOccurrence () =
        let expected = new System.Collections.Generic.Dictionary<string, int>()
        expected.Add("Two",1)
        expected.Add("spaces",1)
        let actual = WordOccurrence.wordOccurrence("Two  spaces")
        Assert.AreEqual(expected, actual)

