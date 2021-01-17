namespace Algorithms.Strings

/// <summary>
/// https://en.wikipedia.org/wiki/String-searching_algorithm#Na%C3%AFve_string_search
///
/// This algorithm tries to find the pattern from every position of
/// the mainString if pattern is found from position i it add it to
/// the answer and does the same for position i+1
/// </summary>
///
/// <remarks>
/// Complexity : O(n*m)
///     n=length of main string
///     m=length of pattern string
/// </remarks>
module NaiveStringSearch =
    /// <summary>
    /// </summary>
    /// <example>
    /// <code>
    /// naive_pattern_search("ABAAABCDBBABCDDEBCABC", "ABC")
    /// [4, 10, 18]
    ///
    /// naive_pattern_search("ABC", "ABAAABCDBBABCDDEBCABC")
    /// []
    ///
    /// naive_pattern_search("", "ABC")
    /// []
    ///
    /// naive_pattern_search("TEST", "TEST")
    /// [0]
    ///
    /// naive_pattern_search("ABCDEGFTEST", "TEST")
    /// [7]
    /// </code>
    /// </example>
    /// <param name="s"></param>
    /// <param name="pattern"></param>
    /// <returns>List of positions</returns>
    let naivePatternSearch (s: string, pattern: string): int list =
        s.ToCharArray()
        |> Seq.mapi
            (fun i x ->
                let myv = pattern.[0]

                if x = pattern.[0] then
                    (i, s.[i..(i + (pattern.Length - 1))])
                else
                    (i, ""))
        |> Seq.where (fun (i, x) -> pattern = x)
        |> Seq.map (fun (i, x) -> i)
        |> List.ofSeq
