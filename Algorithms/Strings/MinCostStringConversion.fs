/// Algorithm for calculating the most cost-efficient sequence for converting one string
/// into another.
/// The only allowed operations are
/// --- Cost to copy a character is copy_cost
/// --- Cost to replace a character is replace_cost
/// --- Cost to delete a character is delete_cost
/// --- Cost to insert a character is insert_cost
///
namespace Algorithms.Strings

module MinCostStringConversion =
    let computeTransformTables
        (
            sourceString: string,
            destinationString: string,
            copyCost: int,
            replaceCost: int,
            deleteCost: int,
            insertCost: int
        ): list<int> * list<string> =
        let sourceSeq = [ sourceString ]
        let destinationSeq = [ destinationString ]
        let lenSourceSeq = sourceSeq.Length
        let lenDestinationSeq = destinationSeq.Length

        let costs =
            [| for i in 0 .. (lenSourceSeq + 1) -> [| for i in 0 .. lenDestinationSeq + 1 -> 0 |] |]

        let ops =
            [| for i in 0 .. lenSourceSeq + 1 -> [| for i in 0 .. lenDestinationSeq + 1 -> "" |] |]

        for i = 1 to lenSourceSeq + 1 do
            costs.[i].[0] <- i * deleteCost
            ops.[i].[0] <- sprintf "D%s" (sourceSeq.[i - 1])

        for i = 1 to lenDestinationSeq + 1 do
            costs.[0].[i] <- i * insertCost
            ops.[0].[i] <- sprintf "I%s" (destinationSeq.[i - 1])

        for i in 1 .. lenSourceSeq + 1 do
            for j in 1 .. lenDestinationSeq + 1 do
                if sourceSeq.[i - 1] = destinationSeq.[j - 1] then
                    costs.[i].[j] <- costs.[i - 1].[j - 1] + copyCost
                    ops.[i].[j] <- sprintf "C%s" (sourceSeq.[i - 1])
                else
                    costs.[i].[j] <- costs.[i - 1].[j - 1] + replaceCost

                    ops.[i].[j] <-
                        sprintf
                            "R%s"
                            (sourceSeq.[i - 1]
                             + (string) (destinationSeq.[j - 1]))

                if costs.[i - 1].[j] + deleteCost < costs.[i].[j] then
                    costs.[i].[j] <- costs.[i - 1].[j] + deleteCost
                    ops.[i].[j] <- sprintf "D%s" (sourceSeq.[i - 1])

                if costs.[i].[j - 1] + insertCost < costs.[i].[j] then
                    costs.[i].[j] <- costs.[i].[j - 1] + insertCost
                    ops.[i].[j] <- sprintf "I%s" (destinationSeq.[j - 1])

        costs |> Seq.cast<int> |> Seq.toList, ops |> Seq.cast<string> |> Seq.toList

    let rec assembleTransformation (ops: list<string>, i: int, j: int): list<string> =
        if i = 0 && j = 0 then
            List.empty
        else
            match ops.[i].[j] with
            | o when o = 'C' || o = 'R' ->
                let mutable seq =
                    assembleTransformation (ops, i - 1, j - 1)
                    |> List.toArray

                let ch =
                    [ ((string) ops.[i].[j]) ] |> List.toArray

                seq <- seq |> Array.append ch
                seq |> List.ofArray
            | 'D' ->
                let mutable seq =
                    assembleTransformation (ops, i - 1, j)
                    |> List.toArray

                let ch =
                    [ ((string) ops.[i].[j]) ] |> List.toArray

                seq <- seq |> Array.append ch
                seq |> List.ofArray
            | _ ->
                let mutable seq =
                    assembleTransformation (ops, i, j - 1)
                    |> List.toArray

                let ch =
                    [ ((string) ops.[i].[j]) ] |> List.toArray

                seq <- seq |> Array.append ch
                seq |> List.ofArray
