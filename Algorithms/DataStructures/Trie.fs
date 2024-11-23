namespace Algorithms.DataStructures

module Trie =

    type Trie = {
        IsWord : bool
        Children : Map<char, Trie>
    }

    let empty : Trie = { IsWord = false; Children = Map.empty }

    let insert (word: string) (trie: Trie) : Trie =
        let rec insertImpl (chars: char list) (trie: Trie) : Trie =
            match chars with
            | [] ->
                { trie with IsWord = true }
            | c :: rest ->
                match trie.Children.TryFind c with
                | Some child ->
                    let child = insertImpl rest child
                    { trie with Children = trie.Children.Add(c, child) }
                | None ->
                    let child = insertImpl rest empty
                    { trie with Children = trie.Children.Add(c, child) }

        insertImpl (word |> Seq.toList) trie

    let search (word: string) (trie: Trie) : bool =
        let rec searchImpl (chars: char list) (trie: Trie) : bool =
            match chars with
            | [] -> trie.IsWord
            | c :: rest ->
                match trie.Children.TryFind c with
                | Some child -> searchImpl rest child
                | None -> false
        searchImpl (word |> Seq.toList) trie

