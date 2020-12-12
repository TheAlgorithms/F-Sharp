namespace Algorithms.Search

open System

module BinarySearch =
    let rec byRecursion (sortedData: IComparable [], item: int, left: int, right: int) =

        let middle = left + (right - left) / 2

        match sortedData.[middle] with
        | s when s.CompareTo(sortedData.[middle]) > item -> byRecursion (sortedData, item, left, middle - 1)
        | s when s.CompareTo(sortedData.[middle]) < item -> byRecursion (sortedData, item, left, middle + 1)
        | _ -> middle

    /// <summary>
    /// Finds index of item in array that equals to item searched for,
    /// time complexity: O(log(n)),
    /// space complexity: O(1),
    /// where n - array size.
    /// </summary>
    /// <param name="sortedData">Sorted array to search in.</param>
    /// <param name="item">Item to search for.</param>
    /// <returns>Index of item that equals to item searched for or -1 if none found.</returns>
    let rec findIndex (sortedData: IComparable [], item: int) =

        let left = 0
        let right = sortedData.Length - 1

        let middle = left + (right - left) / 2
        let currentItem = sortedData.[middle]

        match currentItem with
        | c when c.CompareTo(sortedData.[middle]) > item -> findIndex (sortedData, item)
        | c when c.CompareTo(sortedData.[middle]) < item -> findIndex (sortedData, item)
        | _ -> item
