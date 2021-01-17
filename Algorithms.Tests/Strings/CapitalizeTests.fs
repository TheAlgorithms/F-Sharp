namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type CapitalizeTests () =
    
    [<TestMethod>]
    [<DataRow("hello world", "Hello world")>]
    [<DataRow("123 hello world", "123 hello world")>]
    [<DataRow(" hello world", " hello world")>]
    [<DataRow("a", "A")>]
    [<DataRow("", "")>]
    member this.Capitalize (sentence:string, expected:string) =
        let actual = Capitalize.capitalize sentence
        Assert.AreEqual(expected, actual)

