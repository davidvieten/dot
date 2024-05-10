namespace tests


open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST
open Evaluator
open System.Xml

[<TestClass>]
type TestClass () =

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

    [<TestMethod>]
       member this.twoRoutes() =
           let input = "lefthash net right offense righthash corner left defense"
           let expected = [{ routedef = (LeftHash, Net);
                            dotplace = (Right, Offense) }; 
                            { routedef = (RightHash, Corner);
                            dotplace = (Left, Defense) }]
           let result = parse input
           match result with
           | Some ws ->
               Assert.AreEqual(expected, ws)
           | None ->
               Assert.IsTrue false


    [<TestMethod>]
    member this.EndToEndTest () =
        let input = "lefthash net right offense"
        
        // ensures whitespace is correct in evaluator
        let expectedSVG =
            "<svg width=\"400\" height=\"400\" xmlns=\"http://www.w3.org/2000/svg\">\n" +
            "  <image href=\"Rink.png\" height=\"400\" width=\"400\" />\n" +
            "  <circle cx=\"239\" cy=\"250\" r=\"5\" fill=\"black\" />\n" +
            "  <circle cx=\"200\" cy=\"300\" r=\"5\" fill=\"black\" />\n" +
            "  <line x1=\"239\" y1=\"250\" x2=\"200\" y2=\"300\" style=\"stroke:black;stroke-width:3\" />\n" +
            "</svg>\n"
        
        let result = parse input
        
        match result with
        | Some board ->
            let actualSVG = eval board
            Assert.AreEqual(expectedSVG, actualSVG)
        | None ->
            Assert.Fail("Parsing failed for valid input.")
