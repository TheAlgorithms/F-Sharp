namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type SwapCaseTests () =

    [<TestMethod>]
    [<DataRow("Algorithm.F#@89", "aLGORITHM.f#@89")>]
    member this.swapCase (input:string, expected:string) =
        let actual = SwapCase.swapCase(input)
        Assert.AreEqual(expected, actual)

