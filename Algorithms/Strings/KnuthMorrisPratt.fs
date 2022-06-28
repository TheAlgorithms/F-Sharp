namespace Algorithms.Strings

module KnuthMorrisPratt =
    let getFailureArray (pattern: string) : list<int> =
        let mutable failure = [ 0 ]
        let mutable i = 0
        let mutable j = 1

        while j < pattern.Length do
            if pattern.[i] = pattern.[j] then
                i <- i + 1
                j <- j + 1
                failure <- failure @ [ i ]

            elif i > 0 then
                i <- failure.[i - 1]
            else
                j <- j + 1
                failure <- failure @ [ i ]

        failure

    /// <summary>
    /// The Knuth-Morris-Pratt Algorithm for finding a pattern within a piece of text
    /// with complexity O(n + m)
    /// </summary>
    /// <param name="pattern"></param>
    /// <param name="text"></param>
    /// <returns></returns>
    let kmp (pattern: string, text: string) : bool =
        // 1) Construct the failure array
        let failure = getFailureArray pattern

        let mutable result = false

        // 2) Step through text searching for pattern
        let mutable i, j = 0, 0 // Index into text, pattern

        while i < text.Length do
            if pattern.[j] = text.[i] then
                if j = pattern.Length - 1 && (not result) then
                    i <- text.Length
                    result <- true

                j <- j + 1
                i <- i + 1

            // If this is a prefix in our pattern
            // just go back far enough to continue
            elif j > 0 && (not result) then
                j <- failure.[j - 1]

            else
                i <- i + 1

        result