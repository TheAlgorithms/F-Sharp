namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type ZFunctionTests () =

    [<TestMethod>]
    [<DataRow("abr", "abracadabra",2)>]
    [<DataRow("a", "aaaa", 4)>]
    [<DataRow("xz", "zxxzxxz", 2)>]
    member this.findPattern (pattern:string, inputString:string, expected:int) =
        let actual = ZFunction.findPattern(pattern, inputString)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.zFunction () =
        let expected = [0; 0; 0; 1; 0; 1; 0; 4; 0; 0; 1]
        let actual = ZFunction.zFunction("abracadabra")
        Assert.AreEqual(expected, actual)

