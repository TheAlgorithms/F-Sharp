namespace Algorithms.Strings

open System.Collections.Generic

module WordOccurrence =
    let wordOccurrence (sentence: string): Map<string, int> =
        // Creating a dictionary containing count of each word
        let mutable occurrence: Map<string, int> = Map.empty

        for word in sentence.Split() do
            match word with
            | "" -> ignore word
            | w when occurrence.ContainsKey(word) -> printfn "%A" (word)
            | _ -> occurrence <- occurrence.Add(word, 1)

        occurrence
