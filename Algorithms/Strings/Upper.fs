namespace Algorithms.Strings

module Upper =
    /// <summary>
    /// Will convert the entire string to uppercase letters
    /// </summary>
    /// <param name="input">String to change to uppercase.</param>
    /// <returns>Uppercased string</returns>
    let upper (input: string) =
        let mutable str = ""
        for phrase in input.Split() do
            let mutable word = ""
            for letter in phrase do
                if letter >= 'a' && letter <= 'z'
                then word <- word + (string) ((char) ((int) letter - 32))
                else word <- word + (string) letter
            str <- str + word + " "
        str
