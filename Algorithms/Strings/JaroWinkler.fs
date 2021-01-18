namespace Algorithms.Strings

open Microsoft.FSharp.Collections

module JaroWinkler =
    /// <summary>
    /// Jaro–Winkler distance is a string metric measuring an edit distance between two
    /// sequences.
    /// Output value is between 0.0 and 1.0.
    /// </summary>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    let jaroWinkler (str1: string, str2: string): float =
        let getMatchedCharacters (_str1: string, _str2: string): string =
            let mutable istr1 = _str1
            let mutable istr2 = _str2
            let mutable matched = []
    
            let limit =
                int (floor (double (min _str1.Length str2.Length) / 2.0))
    
            istr1
            |> Seq.iteri
                (fun i l ->
                    let left = int(max 0 (i - limit))
    
                    let right = int(min (i + limit + 1) istr2.Length)
    
                    if (istr2.[left..right - 1]).Contains(l) then
                        matched <- List.append matched [ (string) l ]
                        let myIndex = (istr2.IndexOf(l))
                        istr2 <- $"{istr2.[0..istr2.IndexOf(l) - 1]} {istr2.[istr2.IndexOf(l) + 1..]}")
    
            matched |> List.fold (+) ""
    
        // matching characters
        let matching1 = getMatchedCharacters (str1, str2)
        let matching2 = getMatchedCharacters (str2, str1)
        let matchCount = matching1.Length
        let mutable jaro = 0.0
    
        // Transposition
        let transpositions =
            floor (
                double (
                    (double)
                        [ for c1, c2 in List.zip [ matching1 ] [ matching2 ] do if c1 <> c2 then (c1, c2) ]
                            .Length
                )
            )
    
        if matchCount = 0 then
            jaro <- 0.0
        else
            jaro <-
                1.0 / 3.0
                * ((double) matchCount / (double) str1.Length
                   + (double) matchCount / (double) str2.Length
                   + ((double) matchCount - transpositions)
                     / (double) matchCount)
    
        // Common prefix up to 4 characters
        let mutable prefixLen = 0

        let mutable c1C2BoolList : bool list = []

        if str1.Length = str2.Length then
            for c1, c2 in Array.zip (str1.[..4].ToCharArray()) (str2.[..4].ToCharArray()) do
                if c1 = c2 then
                    c1C2BoolList <- List.append c1C2BoolList [true]
                else
                    c1C2BoolList <- List.append c1C2BoolList [false]
            if (c1C2BoolList |> List.exists(fun x -> (not x))) then
                prefixLen <- prefixLen + (c1C2BoolList |> List.findIndex(fun x -> (not x)))
        jaro + 0.1 * (double) prefixLen * (1.0 - jaro)