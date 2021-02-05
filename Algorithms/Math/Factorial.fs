namespace Algorithms.Math

module Factorial =
    let CalculateFactorial num =
        if (num < 0)
            then failwith "No Factorial for negative numbers"
            else [1..num] |> Seq.fold (fun acc n -> acc * n) 1