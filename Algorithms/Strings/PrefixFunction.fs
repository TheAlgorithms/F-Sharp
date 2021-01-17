namespace Algorithms.Strings

/// https://cp-algorithms.com/string/prefix-function.html
///
/// Prefix function Knuth–Morris–Pratt algorithm
///
/// Different algorithm than Knuth-Morris-Pratt pattern finding
///
/// E.x. Finding longest prefix which is also suffix
/// Time Complexity: O(n) - where n is the length of the string
module PrefixFunction =

    /// <summary>
    /// For the given string this function computes value for each <c>index(i)</c>,
    /// which represents the longest coincidence of prefix and suffix
    /// for given substring <c>inputString[0...i]</c>.
    /// For the value of the first element the algorithm always returns 0
    /// </summary>
    /// <example>
    /// <code>
    /// prefix_function "aabcdaabc"
    /// [0, 1, 0, 0, 0, 1, 2, 3, 4]
    ///
    /// prefix_function("asdasdad")
    /// [0, 0, 0, 1, 2, 3, 4, 0]
    /// </code>
    /// </example>
    /// <param name="inputString"></param>
    /// <returns>A string of <c>int</c></returns>
    let prefixFunction (inputString: string): list<int> =

        // List for the result values
        let mutable prefixResult =
            [| for i in 0 .. (inputString.Length - 1) -> 0 |]

        for i = 1 to (inputString.Length - 1) do
            // Use last results for better performance - dynamic programming
            let mutable j = prefixResult.[i - 1]

            while j > 0 && inputString.[i] <> inputString.[j] do
                j <- prefixResult.[j - 1]

            if inputString.[i] = inputString.[j] then
                j <- j + 1

            prefixResult.SetValue(j, i)

        prefixResult |> List.ofArray

    /// <summary>
    /// Prefix-function use case
    /// Finding longest prefix which is suffix as well
    /// </summary>
    /// <example>
    /// <code>
    /// longest_prefix "aabcdaabc"
    /// 4
    /// longest_prefix "asdasdad"
    /// 4
    /// longest_prefix "abcab"
    /// 2
    /// </code>
    /// </example>
    /// <param name="inputString"></param>
    /// <returns>Returns <c>int</c></returns>
    let longestPrefix (inputString: string): int =
        // Just returning maximum value of the array gives us answer
        prefixFunction (inputString)
        |> System.Linq.Enumerable.Max
