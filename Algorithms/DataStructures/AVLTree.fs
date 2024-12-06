namespace Algorithms.DataStructures

module AVLTree =

    type AVLNode = {
        Value: int
        Height: int
        LeftChild: Option<AVLNode>
        RightChild: Option<AVLNode>
    }

    module AVLNode =
        let create (value: int) : AVLNode =
            {
                Value = value
                Height = 0
                LeftChild = None
                RightChild = None
            }

        let height (maybeNode: Option<AVLNode>) : int =
            maybeNode
            |> Option.map (fun node -> node.Height)
            |> Option.defaultValue -1

        let balanceFactor (node: AVLNode) : int =
            height node.RightChild - height node.LeftChild

    type AVLNode
    with
        member this.UpdateLeftChild (leftChild: Option<AVLNode>) : AVLNode =
            {
                this with
                    LeftChild = leftChild
                    Height = 1 + max (AVLNode.height leftChild) (AVLNode.height this.RightChild)
            }
        member this.UpdateRightChild (rightChild: Option<AVLNode>) : AVLNode =
            {
                this with
                    RightChild = rightChild
                    Height = 1 + max (AVLNode.height this.LeftChild) (AVLNode.height rightChild)
            }

    module AVLTree =
        let rotateRight (node: AVLNode) : AVLNode =
            // Left child becomes the new root
            let leftChild = node.LeftChild |> Option.get
            let oldNode = node.UpdateLeftChild (leftChild.RightChild)
            leftChild.UpdateRightChild (Some oldNode)

        let rotateLeft (node: AVLNode) : AVLNode =
            // Right child becomes the new root
            let rightChild = node.RightChild |> Option.get
            let oldNode = node.UpdateRightChild (rightChild.LeftChild)
            rightChild.UpdateLeftChild (Some oldNode)


    type AVLTree = {
        Root: Option<AVLNode>
    }

    let insert (value: int) (tree: AVLTree) : AVLTree =
        let rec insertImpl (maybeNode: Option<AVLNode>) : AVLNode =
            match maybeNode with
            | None ->
                AVLNode.create value
            | Some node ->
                let node =
                    if value < node.Value then
                        node.UpdateLeftChild (Some (insertImpl node.LeftChild))
                    elif value = node.Value then
                        node
                    else
                        node.UpdateRightChild (Some (insertImpl node.RightChild))

                if AVLNode.balanceFactor node > 1 then
                    // Root node is right heavy, current balance factor is 2
                    // check the balance factor of the right child
                    if node.RightChild |> Option.get |> AVLNode.balanceFactor < 0 then
                        // Right child is left heavy
                        // rotate right around right child and rotate left around root

                        // Illustration: possible heights are shown in brackets,
                        // and the balance factor of the node is shown in parentheses

                        // Initial state:
                        //
                        //                  b (+2) [h+3]
                        //                 / \
                        //           [h]  a   f (-1) [h+2]
                        //                    /\
                        //   [h+1] (0|-1|+1) d  g [h]
                        //                   /\
                        //         [h|h-1]  c  e [h|h-1]

                        // rotate right around f (right child)
                        //
                        //              b (+2) [h+3]
                        //             / \
                        //        [h] a   d (+1|+2) [h+2]
                        //                /\
                        //       [h|h-1] c  f (0|-1) [h+1]
                        //                  /\
                        //         [h|h-1] e  g [h]

                        // rotate left around b (root)
                        //                            d (0) [h+2]
                        //                  __________/\__________
                        //                 /                       \
                        //  [h+1] (0|-1)  b                         f (0|-1) [h+1]
                        //               / \                        /\
                        //          [h] a   c [h|h-1]    [h|h-1]   e  g [h]


                        let node =node.UpdateRightChild (Some (AVLTree.rotateRight (node.RightChild |> Option.get)))
                        AVLTree.rotateLeft node
                    else
                       // Right child is balanced or left heavy,
                       // rotate left around root

                       // Illustration if right child is balanced

                       // Initial state:
                       //              b (+2) [h+3]
                       //             / \
                       //        [h] a   d (0) [h+2]
                       //                /\
                       //        [h+1]  c  e [h+1]

                       // rotate left around b (root)
                       //                d (-1) [h+3]
                       //               / \
                       //   [h+2] (+1) b   e [h+1]
                       //             / \
                       //        [h] a   c [h+1]

                       // Illustration if right child is right heavy

                       // Initial state:
                       //              b (+2) [h+3]
                       //             / \
                       //        [h] a   d (+1) [h+2]
                       //                /\
                       //          [h]  c  e [h+1]

                       // rotate left around b (root)
                       //                d (0) [h+2]
                       //               / \
                       //   [h+1] (0)  b   e [h+1]
                       //             / \
                       //        [h] a   c [h]

                       AVLTree.rotateLeft node
                elif AVLNode.balanceFactor node < -1 then
                    // Root node is left heavy, current balance factor is -2
                    // check the balance factor of the left child
                    if node.LeftChild |> Option.get |> AVLNode.balanceFactor > 0 then
                        // Left child is right heavy
                        // rotate left around left child and rotate right around root

                        // Initial state:
                        //                  f (-2) [h+3]
                        //                 / \
                        //     [h+2] (+1) b   g [h]
                        //                /\
                        //           [h] a  d (0|-1|+1) [h+1]
                        //                  /\
                        //         [h|h-1] c  e [h|h-1]

                        // rotate left around b (left child)
                        //                 f (-2) [h+3]
                        //                / \
                        //    [h+2] (-2) d   g [h]
                        //              / \
                        //       [h+1] b   e [h|h-1]
                        //            /\
                        //       [h] a  c [h|h-1]

                        // rotate right around f (root)
                        //                            d (0) [h+2]
                        //                  __________/\__________
                        //                 /                       \
                        //  [h+1] (0|-1)  b                         f (0|-1) [h+1]
                        //               / \                        /\
                        //          [h] a   c [h|h-1]      [h|h-1] e  g [h]

                        let node = node.UpdateLeftChild (Some (AVLTree.rotateLeft (node.LeftChild |> Option.get)))
                        AVLTree.rotateRight node
                    else
                        // Left child is balanced or left heavy
                        // rotate right around root

                        // Illustration if left child is balanced

                        // Initial state:
                        //              d (-2) [h+3]
                        //             / \
                        //  [h+2] (0) b    e [h]
                        //           / \
                        //    [h+1] a   c [h+1]

                        // rotate right around d (root)
                        //            b (+1) [h+3]
                        //           / \
                        //   [h+1] a    d (-1) [h+2]
                        //             / \
                        //       [h+1]c   e [h]

                        // Illustration if left child is left heavy

                        // Initial state:
                        //              d (-2) [h+3]
                        //             / \
                        // [h+2] (-1) b    e [h]
                        //           / \
                        //    [h+1] a   c [h]

                        // rotate right around d (root)
                        //            b (0) [h+2]
                        //           / \
                        //   [h+1] a    d (0) [h+1]
                        //             / \
                        //        [h] c   e [h]

                        AVLTree.rotateRight node
                else
                    // Balance of root is within acceptable range
                    node


        insertImpl tree.Root
        |> fun root -> { Root = Some root }

