namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type RemoveDuplicatesTests () =

    [<TestMethod>]
    [<DataRow("Python is great and Java is also great", "Python is great and Java also")>]
    [<DataRow("Python   is      great and Java is also great", "Python  is great and Java also")>]
    member this.removeDuplicates (str:string, expected:string) =
        let actual = RemoveDuplicates.removeDuplicates(str)
        Assert.AreEqual(expected, actual)

