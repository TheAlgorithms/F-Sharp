namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type PrefixFunctionTests () =

    [<TestMethod>]
    [<DataRow("aabcdaabc", 4)>]
    [<DataRow("asdasdad", 4)>]
    [<DataRow("abcab", 2)>]
    member this.longestPrefix (input:string, expected:int) =
        let actual = PrefixFunction.longestPrefix(input)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.prefixFunction () =
        let expected = [0; 1; 0; 0; 0; 1; 2; 3; 4]
        let actual = PrefixFunction.prefixFunction("aabcdaabc")
        Assert.AreEqual(expected, actual)

