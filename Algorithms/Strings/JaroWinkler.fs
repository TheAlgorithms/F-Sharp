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
    let jaroWinkler(str1:string, str2:string): float =
        let getMatchedCharacters(_str1:string, _str2:string):string =
            let ref_str2 = ref _str2
            
            let mutable matched = []
            let limit = min _str1.Length ref_str2.Value.Length
            for i in 0 .. _str1.Length - 1 do
                let l = _str1.[i]
                let left = (int)(max 0 (i - limit))
                let right = (int)(min (i + limit + 1) ref_str2.Value.Length)
                if str1.[left .. right].Contains l then
                    let value = (string)l
                    matched <- matched |> List.append [value]
                    ref_str2 := $"{_str2.[0..ref_str2.Value.IndexOf(value)]}{ref_str2.Value.[ref_str2.Value.IndexOf(value) + 1..]}"
            matched |> List.fold (+) ""

        // matching characters
        let matching1 = getMatchedCharacters(str1, str2)
        let matching2 = getMatchedCharacters(str2, str1)
        let matchCount = matching1.Length
        let mutable jaro = 0.0

        // Transposition
        let transpositions =
            float((double)[for c1, c2 in List.zip [matching1] [matching2] -> (c1, c2)].Length / 2.0)

        if matchCount = 0 then
            jaro <- 0.0
        else 
            jaro <- (double)1 / (double)3 * ((double)matchCount / (double)str1.Length + (double)matchCount / (double)str2.Length + ((double)matchCount - transpositions) / (double)matchCount)

        // common prefix up to 4 characters
        let mutable prefixLen = 0
        for c1, c2 in List.zip [str1.[..4]] [str2.[..4]] do
            match c1 with
            | c2 -> prefixLen <- prefixLen + 1
            | _ -> ()

        jaro + 0.1 * (double)prefixLen * (1.0 - jaro)