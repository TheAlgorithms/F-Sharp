namespace Algorithms.Tests.DataStructures

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.DataStructures.Trie

[<TestClass>]
type TrieTests () =

    [<TestMethod>]
    member this.``Test insertion and retrieval with strings``() =
        let mutable trie = empty

        trie <- insert "foo" trie
        Assert.IsTrue(search "foo" trie)

        trie <- insert "foobar" trie
        Assert.IsTrue(search "foobar" trie)
        Assert.IsTrue(search "foo" trie)

        trie <- insert "bar" trie
        Assert.IsTrue(search "bar" trie)
        Assert.IsFalse(search "baz" trie)
        Assert.IsFalse(search "foobarbaz" trie)

    [<TestMethod>]
    member this.``Test empty trie``() =
        let trie = empty
        Assert.IsFalse(search "foo" trie)
        Assert.IsFalse(search "" trie)

    [<TestMethod>]
    member this.``Test insert empty key``() =
        let trie =
            empty
            |> insert ""

        Assert.IsTrue(search "" trie)
        Assert.IsFalse(search "foo" trie)

    [<TestMethod>]
    member this.``Test overlapping keys``() =
        let trie =
            empty
            |> insert "car"
            |> insert "cart"
            |> insert "carter"

        Assert.IsTrue(search "car" trie)
        Assert.IsTrue(search "cart" trie)
        Assert.IsTrue(search "carter" trie)
        Assert.IsFalse(search "care" trie)

    [<TestMethod>]
    member this.``Test partial match``() =
        let trie =
            empty
            |> insert "apple"

        Assert.IsFalse(search "app" trie)
        Assert.IsFalse(search "appl" trie)
        Assert.IsTrue(search "apple" trie)
        Assert.IsFalse(search "applepie" trie)