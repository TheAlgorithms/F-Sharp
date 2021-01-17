namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type ReverseLettersTests () =

    [<TestMethod>]
    [<DataRow("The cat in the hat", "ehT tac ni eht tah")>]
    [<DataRow("The quick brown fox jumped over the lazy dog.", "ehT kciuq nworb xof depmuj revo eht yzal .god")>]
    [<DataRow("Is this true?", "sI siht ?eurt")>]
    [<DataRow("I   love       F#", "I   evol       #F")>]
    member this.reverseLetters (input:string, expected:string) =
        let actual = ReverseLetters.reverseLetters(input)
        Assert.AreEqual(expected, actual)

