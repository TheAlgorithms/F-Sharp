namespace Algorithms.Strings

module Upper =
    /// <summary>
    /// This function will capitalize the first letter of a sentence or a word
    /// </summary>
    /// <param name="sentence">String to capitalize.</param>
    /// <returns>Capitalized string</returns>
    let capitalize (sentence: string) =
        match sentence with
        | "" -> ""
        | s when s.[0] >= 'a' && s.[0] <= 'z' -> sentence.Remove(0, 1).Insert(0, (string) ((char) ((int) s.[0] - 32)))
        | _ -> sentence
