namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type ManacherTests () =

    [<TestMethod>]
    [<DataRow("abbbaba", "abbba")>]
    [<DataRow("ababa", "ababa")>]
    member this.palindromicString (input:string, expected:string) =
        let actual = Manacher.palindromicString(input)
        Assert.AreEqual(expected, actual)

