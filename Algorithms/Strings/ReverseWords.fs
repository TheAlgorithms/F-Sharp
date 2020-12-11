namespace Algorithms.Strings

module ReverseWords =
    /// <summary>
    /// Reverses words in a given string
    /// </summary>
    /// <param name="input">String to reverse.</param>
    /// <returns>Reversed string</returns>
    let reverseWords(input: string) =
        let mutable str = ""
        for i in input.Split() do
            str <- str.Insert(0, i + " ")
        str