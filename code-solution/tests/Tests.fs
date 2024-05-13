namespace tests


open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST
open Evaluator
open Program

[<TestClass>]
type TestClass () =

(*Does the parsar returns the correct AST*)
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
(*Does the parsar returns the correct AST when given more than 1 route*)
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


(*Does the dotplace accurately dictate the coodirnate of the dot*)
    [<TestMethod>]
        member this.TestEvalDot() =
            let expectedDotRightOffense = {x=296; y=250}
            let expectedDotLeftDefense = {x=107; y=280}
            Assert.AreEqual(expectedDotRightOffense, evalDot (Right, Offense))
            Assert.AreEqual(expectedDotLeftDefense, evalDot (Left, Defense))

(*Test for the routestart evaluator*)
    [<TestMethod>]
        member this.TestEvalRouteStart() =
            let input = (RightHash, Hold)
            let expected= {x=353; y=250} 
            let result = evalRouteStart input (Right, Offense)
            Assert.AreEqual(expected, result)

(*Test for the endroute evaluator*)
    [<TestMethod>]
        member this.TestEvalRouteEnd() =
            let input = (RightHash, WalkLine)
            let expected = {x=253; y=250} 
            let result = evalRouteEnd input (Right, Offense)
            Assert.AreEqual(expected, result)

(*Does the route evaluator produce the proper SVG*)
    [<TestMethod>]
        member this.TestEvalRoute() =
            let input = (RightHash, WalkLine)
            let expected = "<circle cx=\"353\" cy=\"250\" r=\"5\" fill=\"black\" />\n" +
                            "<circle cx=\"253\" cy=\"250\" r=\"5\" fill=\"black\" />\n" +
                            "<line x1=\"353\" y1=\"250\" x2=\"253\" y2=\"250\" style=\"stroke:black;stroke-width:3\" />\n"
            let result = evalRoute input (Right, Offense)
            Assert.AreEqual(expected, result)

(*End to end test for a route, checks if an SVG is produced with the proper coordinates*)
    [<TestMethod>]
    member this.EndToEndTest () =
        let input = "lefthash net right offense"
        let expectedSVG =
            "<svg width=\"400\" height=\"400\" xmlns=\"http://www.w3.org/2000/svg\">\n" +
            "<image href=\"Rink.png\" height=\"400\" width=\"400\" />\n" +
            "<circle cx=\"239\" cy=\"250\" r=\"5\" fill=\"black\" />\n" +
            "<circle cx=\"200\" cy=\"300\" r=\"5\" fill=\"black\" />\n" +
            "<line x1=\"239\" y1=\"250\" x2=\"200\" y2=\"300\" style=\"stroke:black;stroke-width:3\" />\n" +
            "</svg>\n"
        
        let result = parse input
        
        match result with
        | Some board ->
            let actualSVG = eval board
            Assert.AreEqual(expectedSVG, actualSVG)
        | None ->
            Assert.Fail("Parsing failed for valid input.")
