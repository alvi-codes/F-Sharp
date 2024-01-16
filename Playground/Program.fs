// Learn more about F# at http://fsharp.org

open System

let print x = 
    printfn "%A" x

let double = ((*)2)
let doubleList = List.map double 

let add x y = x + y

let increment = add 1
let addTen = add 10

let eleven = increment 10
let twenty = addTen 10

let div y x = x / y

let div_2 = div 2

let temp = div_2 10

print temp

let divide x y =
    x / y
let reciprocal = divide 1.0

let out = reciprocal 3


let add' x = 
    let addx y =
        x + y 
    addx

let temp2 = add' 5 2


let add_3 x y z = x + y + z 

let add_3' x =
    let addx y =
        let addy z =
            x + y + z
        addy
    addx

let temp3 = add_3 2 3 5
let temp4 = add_3' 2 3 10


let seven = (add 3) 4


let sqr x = x * x 
let sqr_list = List.map sqr
let result = sqr_list [1..5]

let sqrlist = List.map (fun x -> x * x)
let dummy = sqrlist [1..3]

let inc_list = List.map (fun x -> x + 1)
let dummy2 = inc_list [1; 3; 5]

let gr4test x = x > 4
let is_list_gr4 = List.map gr4test
let dummy3 = is_list_gr4 [1;5;3;9]

let check_if_2 = List.map (fun x -> x = 2)
let dummy4 = check_if_2 [1;2;4;2;5;2]

let lst1 = [1..5]
let lst2 = [6..10]

let makePair a b =
    (a,b)

let pairWithOne = makePair 1 // PairWithOne takes an integer x and returns (1,x)

let combinationsWithOne =    // Map over lst2 to get the first column.
   List.map pairWithOne lst2


let makeColumn lst x =
    let pairWithX = makePair x  // Partially apply makePair.
    List.map pairWithX lst      // Use this to generate a
                                // list of pairs with x
let pairs = List.map (makeColumn lst1) lst2

let allPairs lst1 lst2 =
    let makeColumn lst x =
        List.map (fun y -> (x, y)) lst
    List.map (makeColumn lst2) lst1




let allThings makePairFunc lsta lstb =
    let makeColumn lst x =
        let pairWithX = makePairFunc x    // Partially apply makePairFunc.
        List.map pairWithX lst            // Use this to generate a list of pairs with x
    List.map (makeColumn lstb) lsta

let makePairs a b =
    (a,b)

let makeSum a b =
    a + b

let pair = allThings makePairs lst1 lst2
let sums  = allThings makeSum lst1 lst2


let add2 x y = x + y 

let sum = List.reduce add2

let sum_2 = List.reduce (+)

let lst_temp = [1;5;3;4;7;1]

let res = sum lst_temp

let res_2 = sum_2 lst_temp 

let mult_2 = List.reduce (*)
let res_3 = mult_2 [1;2;3;4]


let fact n = 
    let temp_list = [1..n]
    let mult = List.reduce (*)
    mult temp_list

let fact_2 n = 
    if n = 0
    then 1 
    else List.reduce (*) [1..n]

let test_n = fact 4

let test_n_2 = fact_2 0


let new_list = [ ["Oh";"thoughtless";"mortals! "]; ["ever";"blind"];["to";"fate"] ]
let concat = List.reduce (List.append)
let demo = concat new_list


let allPairs_2 lsta lstb =
    let concatLists lis = if lis = [] then [] else List.reduce List.append lis
    let makeColumn lst x =
        List.map (fun y -> (x,y)) lst
    let lstOfLsts = List.map (makeColumn lstb) lsta
    concatLists lstOfLsts

let aaa = allPairs_2 lst1 lst2

let allPairs_3 lsta lstb =
    let makeColumn lst x =
        List.map (fun y -> (x,y)) lst
    List.collect (makeColumn lstb) lsta
    
let aaa_3 = allPairs_3 lst1 lst2

let checkMax = List.reduce (max) [2.3;5.6;6;9.1]

let replicate x =
    [x;x]
let test = List.collect replicate [5;3;1]

let repl n lst =
    let replicate x =
        List.map (fun _ -> x) [1..n]
    List.collect replicate lst

// let testy = repl 3 [1;2;3]

let repFourLst = repl 5
let testyyy = repFourLst [1;2;3]


let pos_only lst =
    let check x = if x < 0 then [] else [x]
//    List.map check lst
    List.collect check lst

let testyyyyy = pos_only [1;-2;-5;7]


let facto n =
    if n = 0
    then 1.0
    else List.reduce (*) [1.0 .. float n]

let expo x n =
    let term i =
        (x ** (float i)) / (float (facto i))

    [0..n] |> List.map term |> List.reduce (+)


let term x n an =
    an * (x ** (float n))


let poly x = 
    let coeffs = [1.0;0.5;0.0;0.25]
    let term n an =
        an * (x ** (float n))
    List.mapi term coeffs
    |> List.reduce (+)


let answers = List.map poly [0.0..0.1..1.0]



let poly2 coeffs x = 
    let term n an =
        an * (x ** (float n))
    List.mapi term coeffs
    |> List.reduce (+)

let answers2 = List.map (poly2 [1.0;0.5;0.0;0.25]) [0.0..0.1..1.0]



[<EntryPoint>]
let main argv =
    print "Hello World from F#!"

    let str = "test print"
    print str

    print doubleList[3;10;5]
    




    Console.ReadKey() |> ignore // not needed running from Visual Studio
    0 // return an integer exit code
    