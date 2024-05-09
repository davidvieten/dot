namespace tests


open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

    [<TestMethod>]
       member this.ValidPlayReturnsARoute() =
           let input = "lefthash net right offense"
           let expected = [{ routedef = (LeftHash, Net);
                            dotplace = (Right, Offense) }]
           let result = parse input
           match result with
           | Some ws ->
               Assert.AreEqual(expected, ws)
           | None ->
               Assert.IsTrue false