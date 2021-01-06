namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type SplitTests () =

    [<TestMethod>]
    member this.split () =
        let expected =  ["apple"; "banana"; "cherry"; "orange"]
        let actual = Split.split("apple#banana#cherry#orange", '#')
        Assert.AreEqual(expected, actual)

