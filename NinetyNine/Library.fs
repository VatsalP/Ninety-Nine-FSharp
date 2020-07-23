namespace NinetyNine


/// utility functions
module Helper =
    let rec map f =
        function
        | [] -> []
        | x :: xs -> f x :: map f xs

    let rec fold f acc =
        function
        | [] -> acc
        | x :: xs -> fold f (f acc x) xs

    let rev list =
        let rec helper acc =
            function
            | [] -> acc
            | x :: xs -> helper (x :: acc) xs

        helper [] list

    let length list =
        let rec helper count =
            function
            | [] -> count
            | _ :: xs -> helper (count + 1) xs

        helper 0 list

    let replicate list n =
        let rec helper elem acc =
            function
            | 1 -> elem :: acc
            | count -> helper elem (elem :: acc) (count - 1)

        list
        |> fold (fun acc x -> acc @ (helper x [] n)) []

    let rec drop n list =
        match list, n with
        | [], _ -> []
        | _, 0 -> list
        | _ :: xs, _ -> drop (n - 1) xs

    let rec take n list =
        match list, n with
        | [], _
        | _, 0 -> []
        | x :: xs, _ -> x :: take (n - 1) xs

    let slice n m list = list |> drop n |> take (m - n + 1)



/// first ten problems
module First =
    /// last takes a list returns the last element of the list
    let rec last list =
        match list with
        | [] -> None
        | [ x ] -> Some x
        | x :: xs -> last xs

    /// lastTwo takes a list and returns last two elements
    let rec lastTwo list =
        match list with
        | []
        | [ _ ] -> None
        | [ x; y ] -> Some(x, y)
        | _ :: xs -> lastTwo xs

    /// at finds kth element in passed list and returns it
    /// 1 indexed
    let at k =
        let rec helper k list =
            match k, list with
            | _, []
            | 0, _ -> None
            | 1, x :: _ -> Some x
            | _, _ :: xs -> helper (k - 1) xs

        helper k

    /// length returns the length of the list passed
    let length = Helper.length

    /// rev returns passed list reversed
    let rev = Helper.rev

    /// isPalindrome takes a list and checks if its a palindrome or not
    let isPalindrome list = list = rev list


    /// nested list structure
    type Node<'a> =
        | One of 'a
        | Many of list<Node<'a>>

    /// flatten flattens a nested list
    let rec flatten list =
        match list with
        | [] -> []
        | One x :: xs -> x :: flatten xs
        | Many ys :: xs -> flatten ys @ flatten xs

    /// compress take a list and returns a compressed list without duplicates
    let rec compress list =
        match list with
        | [] -> []
        | [ x ] -> [ x ]
        | x :: y :: xs when x = y -> compress (y :: xs)
        | x :: y :: xs -> x :: compress (y :: xs)

    /// pack packs consecutive duplicates of list elements into sublists
    let pack list =
        let rec helper curr acc =
            function
            | [] -> rev acc
            | [ x ] -> rev ((x :: curr) :: acc)
            | x :: y :: xs when x = y -> helper (x :: curr) acc (x :: xs)
            | x :: y :: xs -> helper [] ((x :: curr) :: acc) (y :: xs)

        helper [] [] list

    /// encode returns run length encoding of list passed
    let encode list =
        let inc = (+) 1

        let rec helper curr acc =
            function
            | [] -> rev acc
            | [ x ] -> rev (((inc curr), x) :: acc)
            | x :: y :: xs when x = y -> helper (inc curr) acc (x :: xs)
            | x :: y :: xs -> helper 0 (((inc curr), x) :: acc) (y :: xs)

        helper 0 [] list


/// second set of problems
module Second =
    type Rle<'a> =
        | One of 'a
        | Many of int * 'a

    /// modified rle encode the returns list of Rle type
    let encode list =
        let inc = (+) 1

        let rle x =
            function
            | 1 -> One x
            | count -> Many(count, x)

        let rec helper curr acc =
            function
            | [] -> Helper.rev acc
            | [ x ] -> Helper.rev ((rle x (inc curr)) :: acc)
            | x :: y :: xs when x = y -> helper (inc curr) acc (x :: xs)
            | x :: y :: xs -> helper 0 ((rle x (inc curr)) :: acc) (y :: xs)

        helper 0 [] list

    /// decode takes list a Rle types and returns a decoded list
    let decode list =
        let rec helper acc =
            function
            | One x -> [ x ]
            | Many (1, x) -> x :: acc
            | Many (count, x) -> helper (x :: acc) (Many(count - 1, x))

        list
        |> Helper.fold (fun acc x -> acc @ (helper [] x)) []

    /// duplicate takes a list and returns a new list withe each element duplicated
    let duplicate list = Helper.replicate list 2

    /// replicate takes a list and a count and returns a new list with each elemtent
    /// 
    /// replicated the count no. of times
    let replicate = Helper.replicate


    /// drop every nth element from the list
    let drop list n =
        let rec helper acc list curr =
            match list, curr with
            | [], _ -> Helper.rev acc
            | _ :: xs, 1 -> helper acc xs n
            | x :: xs, _ -> helper (x :: acc) xs (curr - 1)

        helper [] list n

    /// split list into two different parts at n
    let split list n =
        let rec helper fst list c =
            match list, c with
            | [], _ -> Helper.rev fst, []
            | x :: xs, 1 -> Helper.rev (x :: fst), xs
            | x :: xs, _ -> helper (x :: fst) xs (c - 1)

        helper [] list n

    /// slice returns a slice from list passed containing elems between i and k inclusive
    /// 
    /// 0 indexed
    let slice list n m = list |> Helper.slice n m

    /// rotate list n places to the left
    let rotate list n =
        let len = Helper.length list
        let join (a, b) = a @ b
        let reverse (a, b) = b, a
        match n with
        | _ when n < 0 -> split list (len + n) |> reverse |> join
        | _ -> split list n |> reverse |> join
    
    /// removeAt returns list with element removed at k
    /// 
    /// 0 indexed
    let removeAt k list =
        Helper.take k list @ Helper.drop (k + 1) list


/// third set of problems
/// 
/// seed for random is 42
module Third =

    /// insertAt returns list with elem inserted at index i
    /// 
    /// 0 indexed
    let rec insertAt elem i list =
        match list, i with
        | [], _ -> [elem]
        | x :: xs, 1 ->  x :: elem :: xs
        | _, _ when i < 0 -> raise (Failure "negative index")
        | x :: xs, _ -> x :: insertAt elem (i - 1) xs

    /// range returns list with all integers within a given range
    let range n m =
        let rec generate n m acc =
            match n with
            | _ when n = m -> (m :: acc)
            | _ -> generate (n - 1) m (n :: acc)
        match n, m with
        | _  when n >= m -> generate n m [] |> Helper.rev
        | _ -> generate m n []

    /// randSelect extracts list of n randomly selected element from a list
    /// 
    /// uses Fisher–Yates shuffle
    let randSelect list n =
        let rnd = System.Random(42)
        let arr = List.toArray list
        let swap i j =
            let temp = arr.[i]
            Array.set arr i arr.[j]
            Array.set arr j temp
        for i = 0 to ((Array.length arr) - 1) do
            let j = rnd.Next(i, (Array.length arr) - 1)
            swap i j
        Array.toList arr |> Helper.take n

    /// lottoSelect return list with n different random numbers from set of 1 to M
    let lottoSelect n m =
        let rnd = System.Random(42)
        let rec helper n m =
            let draw = rnd.Next(1, m)
            match n with
            | _ when n < 1 -> raise (Failure "n should be postive")
            | 1 -> [ draw ]
            | _ -> draw :: helper (n - 1) m
        helper n m
        
    /// permutaion return random permutation of the elements of list
    let permutation list =
        randSelect list (Helper.length list)

    /// extract generates combination of k distinct elemenets chosed from element list
    let rec extract k list =
        match k with
        | _ when k <= 0 -> [ [] ]
        | _ ->
        begin
            match list with
            | [] -> []
            | x :: xs ->
                let withX = Helper.map (fun y -> x :: y) (extract (k - 1) xs)
                let withoutX = extract (k - 1) xs
                withX @ withoutX
        end