namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type CheckAnagramsTests () =
    
    [<TestMethod>]
    [<DataRow("Silent", "Listen", true)>]
    [<DataRow("This is a string", "Is this a string", true)>]
    [<DataRow("This is    a      string", "Is     this a string", true)>]
    [<DataRow("There", "Their", false)>]
    member this.isAnagram (string1:string, string2:string, expected:bool) =
        let actual = CheckAnagrams.isAnagram (string1, string2)
        Assert.AreEqual(expected, actual)

