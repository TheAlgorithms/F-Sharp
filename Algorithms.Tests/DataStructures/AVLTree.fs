namespace Algorithms.Tests.DataStructures

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.DataStructures.AVLTree

[<TestClass>]
type AVLTreeTests() =

    let rec verifyBalance (maybeNode: Option<AVLNode>) =
        match maybeNode with
        | None -> ()
        | Some node ->
            let bf = AVLNode.balanceFactor node
            Assert.IsTrue(bf >= -1 && bf <= 1, $"Balance factor {bf} is out of range")
            verifyBalance node.LeftChild
            verifyBalance node.RightChild

    let rec verifyHeights (maybeNode: Option<AVLNode>) =
        match maybeNode with
        | None -> ()
        | Some node ->
            let leftHeight = AVLNode.height node.LeftChild
            let rightHeight = AVLNode.height node.RightChild
            Assert.AreEqual(node.Height, 1 + max leftHeight rightHeight)

            verifyHeights node.LeftChild
            verifyHeights node.RightChild

    let rec verifyBST (maybeNode: Option<AVLNode>) : Option< (* Min *) int * (* Max*) int> =
        match maybeNode with
        | None -> None
        | Some node ->
            let maybeLeftMinMax = verifyBST node.LeftChild
            let maybeRightMinMax = verifyBST node.RightChild
            maybeLeftMinMax
            |> Option.iter (fun (_, leftMax) ->
                Assert.IsTrue(leftMax < node.Value, $"Left child {leftMax} is greater than parent {node.Value}")
            )
            maybeRightMinMax
            |> Option.iter (fun (rightMin, _) ->
                Assert.IsTrue(rightMin > node.Value, $"Right child {rightMin} is less than parent {node.Value}")
            )
            let minValue =
                maybeLeftMinMax
                |> Option.map fst
                |> Option.defaultValue node.Value
            let maxValue =
                maybeRightMinMax
                |> Option.map snd
                |> Option.defaultValue node.Value
            Some (minValue, maxValue)

    let verifyProperties (tree: AVLTree) =
        verifyBalance tree.Root
        verifyHeights tree.Root
        verifyBST tree.Root |> ignore
        tree

    [<TestMethod>]
    member _.``Empty tree has no root``() =
        let tree =
            empty
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsNone)

    [<TestMethod>]
    member _.``Insert maintains AVL properties``() =
        let tree =
            empty
            |> verifyProperties
            |> insert 5
            |> verifyProperties
            |> insert 3
            |> verifyProperties
            |> insert 7
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 9
            |> verifyProperties
            |> insert 1
            |> verifyProperties
        ()

    [<TestMethod>]
    member _.``Right-heavy case triggers rotation``() =
        let tree =
            empty
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 2
            |> verifyProperties
            |> insert 3
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)
        let root = tree.Root.Value
        Assert.AreEqual(2, root.Value)
        Assert.AreEqual(Some 1, root.LeftChild |> Option.map (fun n -> n.Value))
        Assert.AreEqual(Some 3, root.RightChild |> Option.map (fun n -> n.Value))

    [<TestMethod>]
    member _.``Left-heavy case triggers rotation``() =
        let tree =
            empty
            |> verifyProperties
            |> insert 3
            |> verifyProperties
            |> insert 2
            |> verifyProperties
            |> insert 1
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)
        let root = tree.Root.Value
        Assert.AreEqual(2, root.Value)
        Assert.AreEqual(Some 1, root.LeftChild |> Option.map (fun n -> n.Value))
        Assert.AreEqual(Some 3, root.RightChild |> Option.map (fun n -> n.Value))

    [<TestMethod>]
    member _.``Double rotation for right-left case``() =
        let tree =
            empty
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 3
            |> verifyProperties
            |> insert 2
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)
        let root = tree.Root.Value
        Assert.AreEqual(2, root.Value)
        Assert.AreEqual(Some 1, root.LeftChild |> Option.map (fun n -> n.Value))
        Assert.AreEqual(Some 3, root.RightChild |> Option.map (fun n -> n.Value))

    [<TestMethod>]
    member _.``Double rotation for left-right case``() =
        let tree =
            empty
            |> verifyProperties
            |> insert 3
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 2
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)
        let root = tree.Root.Value
        Assert.AreEqual(2, root.Value)
        Assert.AreEqual(Some 1, root.LeftChild |> Option.map (fun n -> n.Value))
        Assert.AreEqual(Some 3, root.RightChild |> Option.map (fun n -> n.Value))

    [<TestMethod>]
    member _.``Duplicate insert is idempotent``() =
        let tree1 =
            empty
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 2
            |> verifyProperties

        let tree2 =
            insert 2 tree1
            |> verifyProperties

        Assert.AreEqual(tree1.Root |> Option.map (fun n -> n.Value),
                       tree2.Root |> Option.map (fun n -> n.Value))
    [<TestMethod>]
    member _.``Delete maintains AVL properties``() =
        let tree =
            empty
            |> insert 5
            |> verifyProperties
            |> insert 3
            |> verifyProperties
            |> insert 7
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 9
            |> verifyProperties
            |> insert 4
            |> verifyProperties
            |> insert 6
            |> verifyProperties
            |> insert 8
            |> verifyProperties
            |> insert 2
            |> verifyProperties
            |> delete 5  // Delete root
            |> verifyProperties
            |> delete 1  // Delete leaf
            |> verifyProperties
            |> delete 7  // Delete node with one child
            |> verifyProperties
            |> delete 3  // Delete node with two children
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)

    [<TestMethod>]
    member _.``Delete from empty tree returns empty tree``() =
        let tree =
            empty
            |> delete 1
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsNone)

    [<TestMethod>]
    member _.``Delete non-existent value maintains tree structure``() =
        let tree1 =
            empty
            |> insert 2
            |> verifyProperties
            |> insert 1
            |> verifyProperties
            |> insert 3
            |> verifyProperties

        let tree2 =
            tree1
            |> delete 4
            |> verifyProperties

        Assert.AreEqual(
            tree1.Root |> Option.map (fun n -> n.Value),
            tree2.Root |> Option.map (fun n -> n.Value)
        )

    [<TestMethod>]
    member _.``Complex deletion cases maintain balance``() =
        let tree =
            empty
            |> insert 50  // Create a more complex tree
            |> verifyProperties
            |> insert 25
            |> verifyProperties
            |> insert 75
            |> verifyProperties
            |> insert 10
            |> verifyProperties
            |> insert 35
            |> verifyProperties
            |> insert 60
            |> verifyProperties
            |> insert 90
            |> verifyProperties
            |> insert 5
            |> verifyProperties
            |> insert 15
            |> verifyProperties
            |> insert 30
            |> verifyProperties
            |> insert 40
            |> verifyProperties
            |> insert 55
            |> verifyProperties
            |> insert 65
            |> verifyProperties
            |> insert 80
            |> verifyProperties
            |> insert 95
            |> verifyProperties

            // Test various deletion patterns
            |> delete 50  // Delete root with two children
            |> verifyProperties
            |> delete 25  // Delete inner node with two children
            |> verifyProperties
            |> delete 5   // Delete leaf
            |> verifyProperties
            |> delete 95  // Delete leaf on opposite side
            |> verifyProperties
            |> delete 35  // Delete node with one child
            |> verifyProperties
            |> delete 75  // Delete node requiring rebalancing
            |> verifyProperties

        Assert.IsTrue(tree.Root.IsSome)

    [<TestMethod>]
    member _.``Sequential deletion maintains balance``() =
        let mutable tree = empty

        // Build tree with sequential inserts
        for i in 1..10 do
            tree <- insert i tree
            tree <- verifyProperties tree

        // Delete in reverse order
        for i in seq{10..(-1)..1} do
            tree <- delete i tree
            tree <- verifyProperties tree

        Assert.IsTrue(tree.Root.IsNone)

    [<TestMethod>]
    member _.``Random operations maintain AVL properties``() =
        let rng = System.Random(42)
        let mutable tree = empty

        // Random inserts
        for _ in 1..20 do
            let value = rng.Next(1, 100)
            tree <- insert value tree
            tree <- verifyProperties tree

        // Random deletes
        for _ in 1..10 do
            let value = rng.Next(1, 100)
            tree <- delete value tree
            tree <- verifyProperties tree