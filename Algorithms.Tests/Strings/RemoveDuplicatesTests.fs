namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type RemoveDuplicatesTests () =

    [<TestMethod>]
    [<DataRow("Python is great and Java is also great", "Java Python also and great is")>]
    [<DataRow("Python   is      great and Java is also great", "Java Python also and great is")>]
    member this.removeDuplicates (str:string, expected:string) =
        let actual = RemoveDuplicates.removeDuplicates(str)
        Assert.AreEqual(expected, actual)

