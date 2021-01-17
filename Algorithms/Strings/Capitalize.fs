namespace Algorithms.Strings

module Capitalize =
    /// <summary>
    /// This function will capitalize the first letter of a sentence or a word
    /// </summary>
    /// <example>
    /// <code>
    /// capitalize("hello world")
    /// 'Hello world'
    ///
    /// capitalize("123 hello world")
    /// '123 hello world'
    ///
    /// capitalize(" hello world")
    /// ' hello world'
    ///
    /// capitalize("a")
    /// 'A'
    ///
    /// capitalize("")
    /// ''
    /// </code>
    /// </example>
    /// <param name="sentence">String to capitalize.</param>
    /// <returns>Capitalized string</returns>
    let capitalize (sentence: string) =
        match sentence with
        | "" -> ""
        | s when s.[0] >= 'a' && s.[0] <= 'z' ->
            sentence
                .Remove(0, 1)
                .Insert(0, (string) ((char) ((int) s.[0] - 32)))
        | _ -> sentence
