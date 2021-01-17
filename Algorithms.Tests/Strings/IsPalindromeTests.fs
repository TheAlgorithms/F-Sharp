namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings

[<TestClass>]
type IsPalindromeTests () =
    
    [<TestMethod>]
    [<DataRow("amanaplanacanalpanama", true)>]
    [<DataRow("Hello", false)>]
    [<DataRow("Able was I ere I saw Elba", true)>]
    [<DataRow("racecar", true)>]
    [<DataRow("Mr. Owl ate my metal worm?", true)>]
    member this.isPalindrome (str:string, expected:bool) =
        let actual = IsPalindrome.isPalindrome str
        Assert.AreEqual(expected, actual)

   