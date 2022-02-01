namespace Algorithms.Math

module PerfectNumbers =
    /// Check if a number is perfect. Time complexity: O(√n)
    let isPerfect n =
        match n with
        | n when n <= 0 -> false
        | n ->
            { 1 .. n - 1 }
            |> Seq.takeWhile (fun i -> i * i <= n)
            |> Seq.filter ((%) n >> (=) 0)
            |> Seq.fold (fun acc i -> acc + i + n / i) 0
            |> (=) (2 * n)
