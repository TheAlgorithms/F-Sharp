namespace Strings
open System.Collections.Generic

module WordOccurence =
    let wordOccurence (sentence : string) =
        // Creating a dictionary containing count of each word
        let occurence = new Dictionary<string, int>()
        for word in sentence.Split(" ") do
            match word with
            | w when occurence.ContainsKey(w) -> occurence.Item(w) <- occurence.GetValueOrDefault(w) + 1
            | _ -> occurence.Add(word, 1)
        occurence