namespace Algorithms.Math

module AbsMax =
    let absMax (x: int list) =
        let mutable j = x.[0]

        for i in x do
            if abs i > abs j then j <- i

        j
