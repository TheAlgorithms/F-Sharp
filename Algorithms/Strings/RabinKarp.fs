namespace Algorithms.Strings

module RabinKarp =
    /// Numbers of alphabet which we call base
    let alphabetSize = 256L

    /// Modulus to hash a string
    let modulus = 1000003L

    let nfmod (a: int64, b: int64) =
        let aD = double a
        let bD = double b
        int64 (aD - bD * floor (aD / bD))

    /// <summary>
    /// The Rabin-Karp Algorithm for finding a pattern within a piece of text
    /// with complexity O(nm), most efficient when it is used with multiple patterns
    /// as it is able to check if any of a set of patterns match a section of text in o(1)
    /// given the precomputed hashes.
    /// </summary>
    /// <remarks>
    /// This will be the simple version which only assumes one pattern is being searched
    /// for but it's not hard to modify
    ///
    /// 1) Calculate pattern hash
    ///
    /// 2) Step through the text one character at a time passing a window with the same
    /// length as the pattern
    /// calculating the hash of the text within the window compare it with the hash
    /// of the pattern. Only testing equality if the hashes match
    /// </remarks>
    /// <param name="pattern"></param>
    /// <param name="text"></param>
    /// <returns></returns>
    let rabinKarp (pattern: string, text: string): bool =

        let mutable result = false

        let patLen = pattern.Length
        let textLen = text.Length

        match patLen with
        | p when p > textLen -> false
        | _ ->
            let mutable patternHash = 0L
            let mutable textHash = 0L
            let mutable modulusPower = 1L


            // Calculating the hash of pattern and substring of text
            for i in 0 .. (patLen - 1) do
                patternHash <- (int64 (pattern.[i]) + patternHash * alphabetSize) % modulus
                textHash <- (int64 (text.[i]) + textHash * alphabetSize) % modulus

                if i <> (patLen - 1) then
                    modulusPower <- (modulusPower * alphabetSize) % modulus


            for i in 0 .. (textLen - patLen + 1) do
                if textHash = patternHash
                   && text.[i..i + (patLen - 1)] = pattern then
                    result <- true

                if not result then
                    if i <> (textLen - patLen) then
                        let first =
                            (textHash - int64 (text.[i]) * modulusPower)
                            * alphabetSize

                        let second = int64 (text.[i + patLen])
                        let third = (first + second) % modulus

                        textHash <-
                            ((((textHash - int64 (text.[i]) * modulusPower)
                               * alphabetSize)
                              + int64 (text.[i + patLen])) % modulus)
                            + modulus

            result
