namespace NinetyNine


module Helper =
    let rec map f list =
        match list with
        | [] -> []
        | x :: xs -> (f x) :: map f xs

    let rev list =
        let rec helper acc = function
        | [] -> acc
        | x :: xs -> helper (x :: acc) xs
        helper [] list
    
    let length list =
        let rec helper count = function
        | [] -> count
        | _ :: xs -> helper (count + 1) xs
        helper 0 list


module First =
    // last takes a list returns the last element of the list
    let rec last list = 
        match list with
        | [] -> None
        | [ x ] -> Some x
        | x :: xs -> last xs

    // lastTwo takes a list and returns last two elements
    let rec lastTwo list =
        match list with
        | [] | [ _ ] -> None
        | [ x; y ] -> Some (x, y)
        | _ :: xs -> lastTwo xs

    // at finds kth element in passed list and returns it
    // 1 indexed
    let at k =
        let rec helper k list =
            match k, list with
            | _, [] | 0, _ -> None
            | 1, x :: _ -> Some x
            | _, _ :: xs -> helper (k - 1) xs
        helper k

    // length returns the length of the list passed
    let length = Helper.length

    // rev returns passed list reversed
    let rev = Helper.rev

    // isPalindrome takes a list and checks if its a palindrome or not
    let isPalindrome list =
        list = rev list
    

    // nested list structure
    type Node<'a> =
    | One of 'a 
    | Many of list<Node<'a>>

    // flatten flattens a nested list
    let rec flatten list =
        match list with
        | [] -> []
        | One x :: xs -> x :: flatten xs
        | Many ys :: xs -> flatten ys @ flatten xs

    // compress take a list and returns a compressed list without duplicates
    let rec compress list =
        match list with
        | [] -> []
        | [x] -> [x]
        | x :: y :: xs when x = y -> compress (y :: xs)
        | x :: y :: xs -> x :: compress (y :: xs)

    // pack packs consecutive duplicates of list elements into sublists
    let pack list =
        let rec helper curr acc = function
        | [] -> rev acc
        | [ x ] -> rev ((x :: curr) :: acc)
        | x :: y :: xs when x = y -> helper (x :: curr) acc (x :: xs)
        | x :: y :: xs -> helper [] ((x :: curr) :: acc) (y :: xs)
        helper [] [] list

    // encode returns run length encoding of list passed
    let encode list =
        let inc = (+) 1
        let rec helper curr acc = function
        | [] -> rev acc
        | [ x ] -> rev (((inc curr), x) :: acc)
        | x :: y :: xs when x = y -> helper (inc curr) acc (x :: xs)
        | x :: y :: xs -> helper 0 (((inc curr), x) :: acc) (y :: xs)
        helper 0 [] list
