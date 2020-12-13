namespace Algorithms.Strings

(*
    https://en.wikipedia.org/wiki/String-searching_algorithm#Na%C3%AFve_string_search

    this algorithm tries to find the pattern from every position of
    the mainString if pattern is found from position i it add it to
    the answer and does the same for position i+1

    Complexity : O(n*m)
        n=length of main string
        m=length of pattern string
*)

module NaiveStringSearch =
    let naivePatternSearch (s: string, pattern:string):list<int> =
        let patLen = pattern.Length
        let mutable position = []
        for i in 1.. (s.Length - patLen + 1) do
            let mutable matchFound = true
            for j in 1.. (patLen) do
                if s.[i + j] <> pattern.[j] then
                    matchFound <- false
            if matchFound then
                position <- List.append [i] <| position
        position
