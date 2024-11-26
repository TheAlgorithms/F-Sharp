namespace Algorithms.DataStructures

module Treap =
    type TreapNode = {
        Value: int
        Priority: int64
        Size: int
        LeftChild: Option<TreapNode>
        RightChild: Option<TreapNode>
    }

    module TreapNode =
        let create (value: int) : TreapNode =
            {
                Value = value
                Priority = System.Random().NextInt64()
                Size = 1
                LeftChild = None
                RightChild = None
            }

        let getSize (maybeNode: Option<TreapNode>) : int =
            maybeNode
            |> Option.map (fun node -> node.Size)
            |> Option.defaultValue 0

    type TreapNode
    with
        member this.UpdateLeftChild (leftChild: Option<TreapNode>) : TreapNode =
            {
                this with
                    LeftChild = leftChild
                    Size = 1 + TreapNode.getSize leftChild + TreapNode.getSize this.RightChild
            }
        member this.UpdateRightChild (rightChild: Option<TreapNode>) : TreapNode =
            {
                this with
                    RightChild = rightChild
                    Size = 1 + TreapNode.getSize this.LeftChild + TreapNode.getSize rightChild
            }

    [<RequireQualifiedAccess>]
    type Treap = {
        Root: Option<TreapNode>
    }

    let empty : Treap = { Root = None }

    /// Splits treap into two parts based on value
    /// Returns (left, right) tuple where:
    /// - left contains all nodes with values < split value
    /// - right contains all nodes with values >= split value
    let rec split (root: Option<TreapNode>) (value: int) : Option<TreapNode> * Option<TreapNode> =
        match root with
        | None ->
            None, None
        | Some root ->
            if root.Value < value then
                // root node belongs to the left
                // split the right child of root and update the right child of root
                let updatedRightOfRoot, right = split root.RightChild value
                root.UpdateRightChild updatedRightOfRoot |> Some, right
            else
                // root node belongs to the right
                // split the left child of root and update the left child of root
                let left, updatedLeftOfRoot = split root.LeftChild value
                left, root.UpdateLeftChild updatedLeftOfRoot |> Some

    /// Merges two treaps maintaining BST and heap properties
    /// Assumes all values in left treap are less than all values in right treap
    let rec merge (left: Option<TreapNode>) (right: Option<TreapNode>) : Option<TreapNode> =
        match left, right with
        | None, right -> right
        | left, None -> left
        | Some left, Some right ->
            if left.Priority < right.Priority then
                // left node is the root of the merged treap, merge its right child with right treap
                let updatedLeftsRightChild = merge left.RightChild (Some right)
                left.UpdateRightChild updatedLeftsRightChild |> Some
            else
                // right node is the root of the merged treap, merge its left child with left treap
                let updatedRightsLeftChild = merge (Some left) right.LeftChild
                right.UpdateLeftChild updatedRightsLeftChild |> Some

    // Inserts a new value into the treap, noop if value already exists
    let insert (value: int) (treap: Treap) : Treap =
        let node = TreapNode.create value
        let left, right = split treap.Root value
        let _, right = split right (value + 1)
        merge (merge left (Some node)) right
        |> fun root -> { Root = root }

    let erase (value: int) (treap: Treap) : Treap =
        let left, right = split treap.Root value
        let _, right = split right (value + 1)
        merge left right
        |> fun root -> { Root = root }

    /// Gets the kth smallest element in the treap (0-indexed)
    /// Returns None if k is out of bounds
    let getKthElement (treap: Treap) (k: uint) : Option<int> =
        if TreapNode.getSize treap.Root |> uint <= k then
            None
        else
            let rec getKthElementImpl (root: TreapNode) (k: int) : int =
                assert (k < root.Size)
                if root.Size = 1 then
                    root.Value
                else
                    if k < TreapNode.getSize root.LeftChild then
                        getKthElementImpl (root.LeftChild |> Option.get) k
                    elif k = TreapNode.getSize root.LeftChild then
                        root.Value
                    else
                        getKthElementImpl (root.RightChild |> Option.get) (k - TreapNode.getSize root.LeftChild - 1)
            getKthElementImpl (treap.Root |> Option.get) (int k) |> Some

    /// Gets the index of a value in the treap (0 indexed position in sorted order)
    /// Returns None if value is not found
    let getIndex (treap: Treap) (value: int) : Option<int> =
        let left, right = split treap.Root value
        let node, _right = split right (value + 1)
        node
        |> Option.map (fun _ -> TreapNode.getSize left)