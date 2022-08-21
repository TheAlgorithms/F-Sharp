namespace Algorithms.Math

module Fibonacci =
    let rec PrintSerie (one: int) (two: int) =
        let fibo = one + two
        System.Console.WriteLine fibo
        PrintSerie two fibo
        
    let rec NthFibonacci (n: int): int =
        match n with
            | 0 -> 0
            | 1 -> 1
            | n -> NthFibonacci (n-1) + NthFibonacci (n-2)
