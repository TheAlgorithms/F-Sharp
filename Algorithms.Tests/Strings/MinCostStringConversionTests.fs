namespace Algorithms.Tests.Strings

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Strings
open MinCostStringConversion

[<TestClass>]
type MinCostStringConversionTests () =

    let validateAndApply (source: string ) (operations: Operation array) : string =
        operations
        |> Array.mapFold (fun sourcePosition op ->
            match op with
            | Operation.Copy s ->
                Assert.AreEqual(source.[sourcePosition], s)
                Some s, sourcePosition + 1
            | Operation.Replace (s, d) ->
                Assert.AreEqual(source.[sourcePosition], s)
                Some d, sourcePosition + 1
            | Operation.Delete s ->
                Assert.AreEqual(source.[sourcePosition], s)
                None, sourcePosition + 1
            | Operation.Insert c ->
                Some c, sourcePosition
        ) 0
        |> fst
        |> Array.choose id
        |> Array.map string
        |> String.concat ""

    let calculateCost (operations: Operation array, copyCost:int, replaceCost:int, deleteCost:int, insertCost:int) =
        operations
        |> Array.sumBy (function
            | Operation.Copy _ -> copyCost
            | Operation.Replace _ -> replaceCost
            | Operation.Delete _ -> deleteCost
            | Operation.Insert _ -> insertCost
        )


    [<TestMethod>]
    [<DataRow("", "", 1, 2, 3, 4)>]
    [<DataRow("github", "", 1, 2, 3, 4)>]
    [<DataRow("", "github", 1, 2, 3, 4)>]
    [<DataRow("github", "github", 1, 2, 3, 4)>]
    [<DataRow("banana", "apple", 1, 2, 3, 4)>]
    [<DataRow("banana", "apple", 3, 1, 2, 4)>]
    [<DataRow("banana", "apple", 3, 1, 2, 4)>]
    member this.validateResult (source: string, destination: string, copyCost:int, replaceCost:int, deleteCost:int, insertCost:int) =
        let costs, ops = computeTransformTables (source, destination, copyCost, replaceCost, deleteCost, insertCost)

        for i = 0 to source.Length do
            for j = 0 to destination.Length do
                let sourceSubstring = source.Substring(0, i)
                let destinationSubstring = destination.Substring(0, j)
                let operations = assembleTransformation (ops, i, j)
                let actualDestinationSubstring = validateAndApply sourceSubstring operations
                let calculatedCost = calculateCost (operations, copyCost, replaceCost, deleteCost, insertCost)
                Assert.AreEqual (destinationSubstring, actualDestinationSubstring)
                Assert.AreEqual (costs.[i].[j], calculatedCost)

    static member inputForComputeTransformTables =
            seq {
                yield [|
                    "abbbaba" :> obj
                    "ababa" :> obj
                    1 :> obj
                    2 :> obj
                    3 :> obj
                    3 :> obj
                    ([|
                        [|0; 3; 6; 9; 12; 15|]
                        [|3; 1; 4; 7; 10; 13|]
                        [|6; 4; 2; 5; 8; 11|]
                        [|9; 7; 5; 4; 6; 9|]
                        [|12; 10; 8; 7; 5; 8|]
                        [|15; 13; 11; 9; 8; 6|]
                        [|18; 16; 14; 12; 10; 9|]
                        [|21; 19; 17; 15; 13; 11|]
                    |],
                    [|
                        [|Operation.Copy 'a'; Operation.Insert 'a'; Operation.Insert 'b'; Operation.Insert 'a'; Operation.Insert 'b'; Operation.Insert 'a'|]
                        [|Operation.Delete 'a'; Operation.Copy 'a'; Operation.Insert 'b'; Operation.Copy 'a'; Operation.Insert 'b'; Operation.Copy 'a'|]
                        [|Operation.Delete 'b'; Operation.Delete 'b'; Operation.Copy 'b'; Operation.Insert 'a'; Operation.Copy 'b'; Operation.Insert 'a'|]
                        [|Operation.Delete 'b'; Operation.Delete 'b'; Operation.Copy 'b'; Operation.Replace ('b', 'a'); Operation.Copy 'b'; Operation.Insert 'a'|]
                        [|Operation.Delete 'b'; Operation.Delete 'b'; Operation.Copy 'b'; Operation.Replace ('b', 'a'); Operation.Copy 'b'; Operation.Replace ('b', 'a')|]
                        [|Operation.Delete 'a'; Operation.Copy 'a'; Operation.Delete 'a'; Operation.Copy 'a'; Operation.Delete 'a'; Operation.Copy 'a'|]
                        [|Operation.Delete 'b'; Operation.Delete 'b'; Operation.Copy 'b'; Operation.Delete 'b'; Operation.Copy 'b'; Operation.Delete 'b'|]
                        [|Operation.Delete 'a'; Operation.Copy 'a'; Operation.Delete 'a'; Operation.Copy 'a'; Operation.Delete 'a'; Operation.Copy 'a'|]
                    |]) :> obj
                |]
            }

    [<TestMethod>]
    [<DynamicData(nameof(MinCostStringConversionTests.inputForComputeTransformTables))>]
    member this.computeTransformTables (sourceString:string, destinationString:string, copyCost:int, replaceCost:int, deleteCost:int, insertCost:int, expected:int array array * Operation array array) =
        let actual = MinCostStringConversion.computeTransformTables(sourceString,destinationString,copyCost,replaceCost,deleteCost,insertCost)
        Assert.IsTrue((expected = actual))



