namespace Algorithms.Math

module GreatestCommonDivisor =
    let rec gcd (m: int) (n: int): int = 
      match m,n with
        | 0,n -> n
        | m,n -> gcd (n % m) m
