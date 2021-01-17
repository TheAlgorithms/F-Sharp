namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type ReverseWordsTests () =

    [<TestMethod>]
    [<DataRow("I love F#", "F# love I")>]
    [<DataRow("I Love F#", "F# Love I")>]
    member this.reverseWords (input:string, expected:string) =
        let actual = ReverseWords.reverseWords(input)
        Assert.AreEqual(expected, actual)

