namespace NinetyNine.Tests

open NUnit.Framework
open NinetyNine.Second

[<TestFixture>]
type TestSecond() =

    [<Test>]
    member this.Encode() =
        let expected =
            [ Many(4, "a")
              One "b"
              Many(2, "c")
              Many(2, "a")
              One "d"
              Many(4, "e") ]

        let got =
            encode
                [ "a"
                  "a"
                  "a"
                  "a"
                  "b"
                  "c"
                  "c"
                  "a"
                  "a"
                  "d"
                  "e"
                  "e"
                  "e"
                  "e" ]

        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Decode() =
        let expected =
            [ "a"
              "a"
              "a"
              "a"
              "b"
              "c"
              "c"
              "a"
              "a"
              "d"
              "e"
              "e"
              "e"
              "e" ]

        let got =
            decode
                [ Many(4, "a")
                  One "b"
                  Many(2, "c")
                  Many(2, "a")
                  One "d"
                  Many(4, "e") ]

        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Duplicate() =
        let expected = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
        let got = duplicate ["a";"b";"c";"c";"d"]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Replicate() =
        let expected = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
        let got = replicate ["a";"b";"c"] 3
        Assert.AreEqual(expected, got)
    
    [<Test>]
    member this.Drop() =
        let expected = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
        let got = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Split() =
        let expected = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
        let got = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.SplitSecondEmpty() =
        let expected = (["a"; "b"; "c"; "d"], [])
        let got = split ["a";"b";"c";"d"] 5
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Slice() =
        let expected = ["c"; "d"; "e"; "f"; "g"]
        let got = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Rotate() =
        let expected = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
        let got = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
        Assert.AreEqual(expected, got)
    
    [<Test>]
    member this.RotateNegative() =
        let expected = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
        let got = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.RemoveAt() =
        let expected = ["a"; "c"; "d"]
        let got = removeAt 1 ["a";"b";"c";"d"]
        Assert.AreEqual(expected, got)
