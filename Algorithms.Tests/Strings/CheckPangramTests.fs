namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type CheckPangramTests () =
    
    [<TestMethod>]
    [<DataRow("The quick brown fox jumps over the lazy dog", true)>]
    [<DataRow("Waltz, bad nymph, for quick jigs vex.", true)>]
    [<DataRow("Jived fox nymph grabs quick waltz.", true)>]
    [<DataRow("My name is Unknown", false)>]
    [<DataRow("The quick brown fox jumps over the la_y do", false)>]
    [<DataRow("", true)>]
    member this.CheckPangram (sentence:string, expected:bool) =
        let actual = CheckPangram.checkPangram sentence
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow("The quick brown fox jumps over the lazy dog", true)>]
    [<DataRow("Waltz, bad nymph, for quick jigs vex.", false)>]
    [<DataRow("Jived fox nymph grabs quick waltz.", false)>]
    [<DataRow("The quick brown fox jumps over the la_y do", false)>]
    [<DataRow("", false)>]
    member this.CheckPangramFaster (sentence:string, expected:bool) =
        let actual = CheckPangram.checkPangramFaster sentence
        Assert.AreEqual(expected, actual)

