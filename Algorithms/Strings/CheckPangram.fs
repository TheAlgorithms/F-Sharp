namespace Algorithms.Strings

/// wiki: https://en.wikipedia.org/wiki/Pangram
module CheckPangram =
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

    let checkPangram (inputString: string): bool =
        let mutable frequency = Set.empty
        let inputStr = inputString.Replace(" ", "") // Replacing all the whitespace in our sentence

        for alpha in inputStr do
            if 'a' <= alpha.Lower() && alpha.Lower() <= 'z' then
                frequency <- frequency.Add(alpha.Lower())

        match frequency.Count with
        | 26 -> true
        | _ -> if inputStr = "" then true else false

    let checkPangramFaster (inputString: string): bool =
        let mutable flag = [| for i in 1 .. 26 -> false |]

        for char in inputString do
            if char.IsLower() then
                flag.SetValue(true, (int) char - (int) 'a')

        flag |> Array.forall (id)
