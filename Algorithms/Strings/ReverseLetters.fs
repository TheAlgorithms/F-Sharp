namespace Algorithms.Strings

module ReverseLetters =
    /// <summary>
    /// Reverses letters in a given string without adjusting the position of the words
    /// </summary>
    /// <param name="input">String to reverse.</param>
    /// <returns>Reversed string</returns>
    let reverseLetters(input: string) =
        let mutable str = ""
        for phrase in input.Split() do
            let mutable word = ""
            for letter in phrase do
                word <- word.Insert(0, (string)letter)
            str <- str + word + " "
        str
