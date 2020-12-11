namespace Algorithms.Math

module Perfect_Numbers =
    let IsPerfect (number: int) =
        let mutable total = 0
        for num in 1..number - 1 do
            if number % num = 0 then
                total <- total + number
        let res = total = number
        res
