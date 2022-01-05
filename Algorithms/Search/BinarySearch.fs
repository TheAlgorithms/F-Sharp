namespace Algorithms.Search

module BinarySearch =
    /// Search the target item in a sorted array.
    /// Returns -1 when the target is not found.
    /// Time complexity: O(log(sortedData.Length)))
    let findIndex target sortedData =
        let rec search l r =
            let count = r - l
            let mid = (l + r) / 2
            let midItem = Array.item mid sortedData

            match count <= 1, compare midItem target with
            | _, 0 -> mid
            | true, _ -> -1
            | false, -1 -> search mid r
            | false, 1 -> search l mid
            | _ -> exn () |> raise

        search 0 (Array.length sortedData + 1)
