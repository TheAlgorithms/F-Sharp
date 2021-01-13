namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type JaroWinklerTests () =
    [<TestMethod>]
    [<DataRow("martha", "marhta", 0.9611111111111111)>]
    [<DataRow("CRATE", "TRACE", 0.8483333333333334)>]
    [<DataRow("test", "dbdbdbdb", 0.0)>]
    [<DataRow("hello world", "HeLLo W0rlD", 0.7131313131313132)>]
    [<DataRow("test", "", 0.0)>]
    [<DataRow("hello", "world", 0.49)>]
    [<DataRow("hell**o", "*world", 0.373015873015873)>]
    member this.jaroWinkler (str1:string, str2:string, expected:float) =
        let actual = JaroWinkler.jaroWinkler(str1, str2)
        Assert.AreEqual(expected, actual)

