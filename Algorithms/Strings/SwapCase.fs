namespace Algorithms.Strings

module SwapCase =
    type System.Char with
        member this.IsUpper(): bool =
            match this with
            | c when c >= 'A' && c <= 'Z' -> true
            | _ -> false

        member this.IsLower(): bool =
            match this with
            | c when c >= 'a' && c <= 'z' -> true
            | _ -> false

        member this.Lower(): char =
            match this with
            | c when c >= 'A' && c <= 'Z' -> (char) ((int) this + 32)
            | _ -> this

        member this.Upper(): char =
            match this with
            | c when c >= 'a' && c <= 'z' -> (char) ((int) this - 32)
            | _ -> this

    /// <summary>
    /// This function will convert all lowercase letters to uppercase letters and vice versa
    /// </summary>
    let swapCase (sentence: string): string =
        let mutable newString = ""
        for character in sentence do
            match character with
            | c when c.IsUpper() -> newString <- newString + (string) (character.Lower())
            | c when c.IsLower() -> newString <- newString + (string) (character.Upper())
            | _ -> newString <- newString + (string) character
        newString
