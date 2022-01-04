namespace Algorithms.Strings

module Manacher =
    type List<'a> with
        member this.LengthInDouble = (double) this.Length

    let palindromicString (inputString: string): string =

        let mutable maxLength = 0

        // If input_string is "aba" than new_input_string become "a|b|a"
        let mutable newInputString = ""
        let mutable outputString = ""

        // Append each character + "|" in new_string for range(0, length-1)
        for i in inputString.[..(inputString.Length - 1)] do
            newInputString <- newInputString + string (i) + "|"

        printfn "%A" newInputString
        // Append last character
        newInputString <-
            newInputString
            + string inputString.[inputString.Length - 1]

        // We will store the starting and ending of previous furthest ending palindromic
        // substring
        let mutable l, r = 0, 0

        // length.[i] shows the length of palindromic substring with center i
        let length =
            [ for i in 0 .. newInputString.Length -> 1 ]
            |> Array.ofList



        let mutable start = 0

        // For each character in new_string find corresponding palindromic string
        for i in 0 .. newInputString.Length do
            // k = 1 if i > r else min(length[l + r - i] // 2, r - i + 1)
            let mutable k =
                if i > r then
                    1
                else
                    min ((int) (floor ([ l + r - 1 ].LengthInDouble / 2.0))) (r - i + 1)

            while i - k >= 0
                  && i + k < newInputString.Length
                  && newInputString.[k + i] = newInputString.[i - k] do
                k <- k + 1

                length.[i] <- 2 * k - 1

                // Does this string end after the previously explored end (that is r) ?
                // if yes the update the new r to the last index of this
                if i + k - 1 > r then
                    l <- i - k + 1 // noqa: E741
                    r <- i + k - 1

                // update max_length and start position
                if maxLength < length.[i] then
                    maxLength <- length.[i]
                    start <- i

        // create that string
        let s =
            newInputString.[int (floor (((double) start - (double) maxLength / 2.0)))..(int) (floor ((double) start + (double) maxLength / 2.0 + 1.0))]

        for i in s do
            if i <> '|' then
                outputString <- outputString + (string) i


        outputString
