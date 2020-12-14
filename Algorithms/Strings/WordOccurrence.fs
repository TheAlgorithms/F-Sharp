namespace Algorithms.Strings

open System.Collections.Generic

module WordOccurrence =
    let wordOccurrence (sentence: string) =
        // Creating a dictionary containing count of each word
        let occurrence = new Dictionary<string, int>()
        for word in sentence.Split(" ") do
            match word with
            | w when occurrence.ContainsKey(w) -> occurrence.Item(w) <- occurrence.GetValueOrDefault(w) + 1
            | _ -> occurrence.Add(word, 1)
        occurrence
