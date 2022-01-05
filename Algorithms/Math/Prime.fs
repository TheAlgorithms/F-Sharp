namespace Algorithms.Math

module Prime =
    /// Check if a number is prime. Time complexity: O(√n)
    let isPrime n =
        if n <= 1 then
            false
        elif n = 2 then
            true
        elif n % 2 = 0 then
            false
        else
            seq {
                let mutable i = 3

                while i * i <= n do
                    yield i
                    i <- i + 2
            }
            |> Seq.forall ((%) n >> (<>) 0)
