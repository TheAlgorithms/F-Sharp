namespace Algorithms.Strings

module RabinKarp =
    /// Numbers of alphabet which we call base
    let alphabetSize = 256

    /// Modulus to hash a string
    let modulus = 1000003

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
    let rabitKarp (pattern: string, text: string): bool =

        let mutable result = false

        let patLen = pattern.Length
        let textLen = text.Length
        match patLen with
        | p when p > textLen -> false
        | _ ->
            let mutable patternHash = 0
            let mutable textHash = 0
            let mutable modulusPower = 1

            // Calculating the hash of pattern and substring of text
            for i = 0 to patLen do
                patternHash <- ((int) (pattern.[i]) + patternHash * alphabetSize) % modulus
                textHash <- (((int) text.[i]) + textHash * alphabetSize) % modulus

                modulusPower <- (modulusPower * alphabetSize) % modulus

            for i = 0 to (textLen - patLen + 1) do
                match i with
                | i when textHash = patternHash
                         && text.[i + patLen] = (char) pattern -> result <- true
                | _ -> result <- false

                // Calculate the https://en.wikipedia.org/wiki/Rolling_hash
                textHash <-
                    (textHash - (int) (text.[i]) * modulusPower)
                    * alphabetSize
                    + (int) (text.[i + patLen]) % modulus
            result
