namespace Strings

module ReverseLetters =
    /// <summary>
    /// Reverses letters in a given string without adjusting the position of the words
    /// </summary>
    /// <param name="input">String to reverse.</param>
    /// <returns>Reversed string</returns>
    let reverseLetters(input: string) =
        let mutable str = ""
        for i in input do
            str <- str.Insert(0, i.ToString())
        str
