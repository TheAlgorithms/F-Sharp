namespace Algorithms.Tests.DataStructures

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.DataStructures.Treap

[<TestClass>]
type TreapTests () =

    [<TestMethod>]
    member this.``Test basic operations``() =
        let treap =
            empty
            |> insert 5
            |> insert 3
            |> insert 7

        // Test getKthElement (0-based indexing)
        Assert.AreEqual(Some 3, getKthElement treap 0u)
        Assert.AreEqual(Some 5, getKthElement treap 1u)
        Assert.AreEqual(Some 7, getKthElement treap 2u)
        Assert.AreEqual(None, getKthElement treap 3u)

        // Test getIndex
        Assert.AreEqual(Some 0, getIndex treap 3)
        Assert.AreEqual(Some 1, getIndex treap 5)
        Assert.AreEqual(Some 2, getIndex treap 7)
        Assert.AreEqual(None, getIndex treap 4)

    [<TestMethod>]
    member this.``Test empty treap``() =
        let treap = empty
        Assert.AreEqual(None, getKthElement treap 0u)
        Assert.AreEqual(None, getIndex treap 5)

    [<TestMethod>]
    member this.``Test deletion``() =
        let mutable treap =
            empty
            |> insert 5
            |> insert 3
            |> insert 7

        // Delete middle element
        treap <- erase 5 treap
        Assert.AreEqual(Some 3, getKthElement treap 0u)
        Assert.AreEqual(Some 7, getKthElement treap 1u)
        Assert.AreEqual(None, getKthElement treap 2u)

        // Delete non-existent element
        treap <- erase 5 treap
        Assert.AreEqual(Some 3, getKthElement treap 0u)
        Assert.AreEqual(Some 7, getKthElement treap 1u)

    [<TestMethod>]
    member this.``Test duplicate insertion``() =
        let mutable treap = empty
        treap <- insert 3 treap
        treap <- insert 5 treap
        treap <- insert 1 treap
        treap <- insert 3 treap

        Assert.AreEqual(Some 1, getKthElement treap 0u)
        Assert.AreEqual(Some 3, getKthElement treap 1u)
        Assert.AreEqual(Some 5, getKthElement treap 2u)
        Assert.AreEqual(None, getKthElement treap 3u)

        treap <- erase 3 treap
        Assert.AreEqual(Some 1, getKthElement treap 0u)
        Assert.AreEqual(Some 5, getKthElement treap 1u)
        Assert.AreEqual(None, getKthElement treap 2u)


    [<TestMethod>]
    member this.``Test order preservation``() =
        let mutable treap = empty

        // Insert in non-sorted order
        treap <- insert 8 treap
        treap <- insert 3 treap
        treap <- insert 10 treap
        treap <- insert 1 treap
        treap <- insert 6 treap
        treap <- insert 4 treap
        treap <- insert 7 treap
        treap <- insert 14 treap
        treap <- insert 13 treap

        // Verify elements are retrievable in sorted order
        for i in 0u..8u do
            let expected =
                match i with
                | 0u -> 1 | 1u -> 3 | 2u -> 4 | 3u -> 6 | 4u -> 7
                | 5u -> 8 | 6u -> 10 | 7u -> 13 | 8u -> 14
                | _ -> failwith "Unexpected index"
            Assert.AreEqual(Some expected, getKthElement treap i)
