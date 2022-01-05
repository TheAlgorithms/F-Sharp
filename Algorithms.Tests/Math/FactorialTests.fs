namespace Algorithms.Tests.Math

open Microsoft.VisualStudio.TestTools.UnitTesting
open Algorithms.Math

[<TestClass>]
type FactorialTests() =

    [<TestMethod>]
    [<DataRow(0, 1)>]
    [<DataRow(1, 1)>]
    [<DataRow(2, 2)>]
    [<DataRow(3, 6)>]
    [<DataRow(5, 120)>]
    [<DataRow(8, 40320)>]
    [<DataRow(10, 3628800)>]
    [<DataRow(12, 479001600)>]
    member this.ValidInt(num: int, expected: int) =
        seq {
            Factorial.byFoldFunction
            Factorial.byReduceFunction
            Factorial.byRecursion
            Factorial.byTailRecursion
            Factorial.byTailRecursionGeneric
        }
        |> Seq.iter (fun func ->
            let actual = func num
            Assert.AreEqual(expected, actual))

    [<TestMethod>]
    [<DataRow(-1)>]
    member this.InvalidInt(num: int) =
        seq {
            Factorial.byFoldFunction
            Factorial.byReduceFunction
            Factorial.byRecursion
            Factorial.byTailRecursion
            Factorial.byTailRecursionGeneric
        }
        |> Seq.iter (fun func ->
            let action =
                new System.Action(fun () -> func num |> ignore)

            Assert.ThrowsException(action) |> ignore)

    [<TestMethod>]
    member this.Generic() =
        Assert.AreEqual(479001600, Factorial.byTailRecursionGeneric 12)
        Assert.AreEqual(479001600u, Factorial.byTailRecursionGeneric 12u)
        Assert.AreEqual(479001600.f, Factorial.byTailRecursionGeneric 12.f)
        Assert.AreEqual(2432902008176640000L, Factorial.byTailRecursionGeneric 20L)
        Assert.AreEqual(2432902008176640000UL, Factorial.byTailRecursionGeneric 20UL)
        Assert.AreEqual(2432902008176640000., Factorial.byTailRecursionGeneric 20.)
        Assert.AreEqual(10888869450418352160768000000M, Factorial.byTailRecursionGeneric 27M)

        Assert.AreEqual(
            30414093201713378043612608166064768844377641568960512000000000000I,
            Factorial.byTailRecursionGeneric 50I
        )
