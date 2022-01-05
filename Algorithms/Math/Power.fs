namespace Algorithms.Math

module Power =
    /// Calculates x^n. Time complexity: O(√n).
    let byFoldFunction x n =
        { 1 .. n } |> Seq.fold (fun acc _ -> acc * x) 1

    /// Calculates x^n. x and n can be negative.
    /// Time complexity O(n).
    let rec byRecursion x n =
        match x, sign n with
        | 0, -1 ->
            System.DivideByZeroException "Attempted to divide by zero."
            |> raise
        | -1, _ -> if n % 2 = 0 then 1 else -1
        | 1, _ -> 1
        | _, 0 -> 1
        | 0, _ -> 0
        | _, -1 -> 0
        | _, _ -> x * byRecursion x (n - 1)
