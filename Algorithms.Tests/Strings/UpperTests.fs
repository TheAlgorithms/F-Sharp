namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type UpperTests () =

    [<TestMethod>]
    [<DataRow("wow", "WOW")>]
    [<DataRow("Hello", "HELLO")>]
    [<DataRow("WHAT", "WHAT")>]
    [<DataRow("wh[]32", "WH[]32")>]
    member this.upper (input:string, expected:string) =
        let actual = Upper.upper(input)
        Assert.AreEqual(expected, actual)

