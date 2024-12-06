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