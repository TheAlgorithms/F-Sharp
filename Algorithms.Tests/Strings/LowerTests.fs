namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type LowerTests () =

    [<TestMethod>]
    [<DataRow("wow", "wow")>]
    [<DataRow("HellZo", "hellzo")>]
    [<DataRow("WHAT", "what")>]
    [<DataRow("wh[]32", "wh[]32")>]
    [<DataRow("whAT", "what")>]
    member this.lower (input:string, expected:string) =
        let actual = Lower.lower(input)
        Assert.AreEqual(expected, actual)

