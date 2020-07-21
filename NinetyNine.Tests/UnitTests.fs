module NinetyNine.Tests

open NUnit.Framework
open NinetyNine.First


[<TestFixture>]
type Testen() =

    [<Test>]
    member this.LastSome() =
        let expected = Some "d"
        let got = last [ "a"; "b"; "c"; "d" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Lastempty() =
        let expected = None
        let got = last []
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.LastTwoSome() =
        let expected = Some("c", "d")
        let got = lastTwo [ "a"; "b"; "c"; "d" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.AtSome() =
        let expected = Some "c"
        let got = at 3 [ "a"; "b"; "c"; "d"; "e" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.AtNone() =
        let expected = None
        let got = at 2 [ "a" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Length() =
        let expected = 3
        let got = length [ "a"; "b"; "c" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.LengthZero() =
        let expected = 0
        let got = length []
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Rev() =
        let expected = [ "a"; "b"; "c" ]
        let got = rev [ "c"; "b"; "a" ]
        Assert.AreEqual(expected, got)

    [<Test>]
    member this.IsPalindrome() =
        let expected = [ "x"; "a"; "m"; "a"; "x" ]
        let got = isPalindrome expected
        Assert.IsTrue(got)

    [<Test>]
    member this.IsPalindromeNot() =
        let expected = [ "x"; "a"; "m" ]
        let got = isPalindrome expected
        Assert.IsTrue(not got)

    [<Test>]
    member this.Flatten() =
        let expected = [ "a"; "b"; "c"; "d"; "e" ]

        let got =
            flatten
                [ One "a"
                  Many
                      [ One "b"
                        Many [ One "c"; One "d" ]
                        One "e" ] ]

        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Compress() =
        let expected = [ "a"; "b"; "c"; "a"; "d"; "e" ]

        let got =
            compress
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
    member this.Pack() =
        let expected =
            [ [ "a"; "a"; "a"; "a" ]
              [ "b" ]
              [ "c"; "c" ]
              [ "a"; "a" ]
              [ "d"; "d" ]
              [ "e"; "e"; "e"; "e" ] ]

        let got =
            pack
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
                  "d"
                  "e"
                  "e"
                  "e"
                  "e" ]

        Assert.AreEqual(expected, got)

    [<Test>]
    member this.Encode() =
        let expected =
            [ (4, "a")
              (1, "b")
              (2, "c")
              (2, "a")
              (1, "d")
              (4, "e") ]

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
