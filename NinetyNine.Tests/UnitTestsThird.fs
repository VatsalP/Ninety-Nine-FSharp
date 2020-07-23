namespace NinetyNine.Tests

open NUnit.Framework
open NinetyNine.Third

[<TestFixture>]
type TestThird() =

    [<Test>]
    member this.InsertAt() =
        let expected = ["a"; "alfa"; "b"; "c"; "d"]
        let got = insertAt "alfa" 1 ["a";"b";"c";"d"]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.InsertAtSecond() =
        let expected = ["a"; "b"; "c"; "alfa"; "d"]
        let got = insertAt "alfa" 3 ["a";"b";"c";"d"]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.InsertAtThird() =
        let expected = ["a"; "b"; "c"; "d"; "alfa"]
        let got = insertAt "alfa" 4 ["a";"b";"c";"d"]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.RangePostive() =
        let expected = [4; 5; 6; 7; 8; 9]
        let got =  range 4 9
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.RangeNegative() =
        let expected = [9; 8; 7; 6; 5; 4]
        let got = range 9 4
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Extract() =
        let expected = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
        let got = extract 2 ["a"; "b"; "c"; "d"]
        Assert.AreEqual(expected, got)