namespace Math

module Fibonacci =
    let rec PrintSerie (one: int) (two: int) =
        let fibo = one + two
        System.Console.WriteLine fibo
        PrintSerie two fibo