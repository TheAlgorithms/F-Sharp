namespace Algorithms.Sort

module CombSort =
    let Sort list: 'T [] =
        let mutable list = list |> Array.copy
        let mutable gap = double list.Length
        let mutable swaps = true
        while gap > 1.0 || swaps do
            gap <- gap / 1.247330950103979
            if gap < 1.0 then gap <- 1.0
            let mutable i = 0
            swaps <- false
            while i + int gap < list.Length do
                let igap = i + int gap
                if list.[i] > list.[igap] then
                    let swap = list.[i]
                    list.[i] <- list.[igap]
                    list.[igap] <- swap
                    swaps <- true
                i <- i + 1
        list
