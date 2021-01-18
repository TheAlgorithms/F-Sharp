namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type JaroWinklerTests () =
    [<TestMethod>]
    [<DataRow("martha", "marhta", 0.9611111111111111)>]
    [<DataRow("CRATE", "TRACE", 0.7333333333333334)>]
    [<DataRow("test", "dbdbdbdb", 0.0)>]
    [<DataRow("test", "test", 1.0)>]
    [<DataRow("hello world", "HeLLo W0rlD", 0.6363636363636364)>]
    [<DataRow("test", "", 0.0)>]
    [<DataRow("hello", "world", 0.4666666666666666)>]
    [<DataRow("hell**o", "*world", 0.4365079365079365)>]
    member this.jaroWinkler (str1:string, str2:string, expected:float) =
        let actual = JaroWinkler.jaroWinkler(str1, str2)
        Assert.AreEqual(expected, actual)
