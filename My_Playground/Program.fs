﻿

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

/// test XML
let pos_only lst =
    let check x = if x < 0 then [] else [x]
//    List.map check lst
    List.collect check lst

let testyyyyy = pos_only [1;-2;-5;7]

let x = 2
let name = "Brown"
// old syntax
printfn "x is %d, name is %s" x name
//string interpolation (no type checking of x, name)
printfn $"x is {x}, name is {name}"
// string interpolation with format specifier
printfn $"x is {x}, name is %s{name}"


let sumAndMean lst =
    let length = float (List.length lst)
    let sum = float (List.reduce (+) lst)
    sum , (sum / length)

let testing = sumAndMean [3;5;2]


let calcHyp (x,y) =
    sqrt <| (x ** 2.0 + y ** 2.0)
    
let tri1 = 3.0 , 4.0
let hyp1 = calcHyp tri1







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


module lens =
    type Lens<'A, 'B> = ('A -> 'B) * ('B -> 'A -> 'A)
    type XYPos = { X: float; Y: float }
    type Rectangle = { TopLeft: XYPos; BottomRight: XYPos }
    let l1: Lens<Rectangle, XYPos> = (fun rect -> rect.TopLeft), (fun tl rect -> { rect with TopLeft = tl })
    let l2: Lens<XYPos, float> = (fun xy -> xy.X), (fun x xy -> { xy with X = x })

    // Using the types and lenses defined earlier

    // Initial rectangle
    let initialRectangle = { TopLeft = { X = 1.0; Y = 2.0 }; BottomRight = { X = 5.0; Y = 6.0 } }
    printfn "Initial Rectangle: %A" initialRectangle

    // Using l1 to get the TopLeft point
    let topLeftPoint = fst l1 initialRectangle
    printfn "TopLeft Point: %A" topLeftPoint

    // Using l2 to get the X-coordinate of the TopLeft point
    let xCoordinate = fst l2 topLeftPoint
    printfn "X-coordinate of TopLeft Point: %f" xCoordinate

    // Using l2 to update the X-coordinate of the TopLeft point
    let updatedTopLeft = snd l2 10.0 topLeftPoint
    printfn "Updated TopLeft Point: %A" updatedTopLeft

    // Using l1 to update the TopLeft point of the rectangle
    let updatedRectangle = snd l1 updatedTopLeft initialRectangle
    printfn "Updated Rectangle: %A" updatedRectangle



let lst1x = [1 ; -1 ; 6 ; 0 ; -3]
let lst2x = [2 ; 5 ; 65 ; 3]
    
let firstNegative = List.tryFind ((>) 0) // Why is this not <?
    
let res1 = firstNegative lst1x
let res2 = firstNegative lst2x

let foo lst =
    List.tryFind ((>) 0) lst
    |> Option.orElse (List.tryFind ((<) 0) lst)
    |> Option.defaultValue 0

let ttt = foo []





let lst = [1;2;3;4]
    
match lst with
| hd::tl -> printfn "This is the head: %d and This is the tail: %A" hd tl
| [] -> printfn "This is an empty list"


match lst with
| fst::(snd::rest) -> printfn "First element: %d\nSecond Element: %d\n Rest:%A" fst snd rest
| fst::rest -> printfn "Only one element in input list"
| [] -> printfn "Empty input list"


// Construct function to sum
let ssum = List.fold (+) 0
    
let rres1 = ssum [1;5;-3]  // = 3
let rres2 = ssum []        // = 0

let rev lstx =
    List.fold (fun new_lst el -> el::new_lst) [] lstx

let dumy = rev lst

let tet = List.fold (fun n s -> n + (String.length s)) 0 ["a";"bc"; "def"]
// strongly recommended alternate form for List.fold usage
// (initState,lstToFold) ||> List.fold (fun oldState element -> newState)
let tet2 = (0, ["a" ; "bc" ; "def"]) ||> List.fold (fun len str -> str.Length + len)


let a : Map<string,int> = Map.empty // create an empty Map value

let a' = Map.add "first" 10 a // create an updated Map - NB this does not change a

let a'' = Map.add "second" 20 a'

let b = Map.ofList [ "April",30 ; "June",30 ; "September",30 ; "November",30 ; "February",28 ]

let ttttt = a''["first"]
let tttttt = a''["second"]

let a''' = Map.add "" 30 a''
let tttttttt = a'''[""]

let months = Map.ofList [ "April",30 ; "June",30 ; "September",30 ; "November",30 ; "February",28 ]
let txt = Map.toList months 

/// testing XML
let inverseMap m =
    m
    |> Map.toList
    |> List.map (fun (k,v) -> (v,k))
    |> Map.ofList

let inv_map = inverseMap months

let data = "The quick brown fox jumps over the lazy dog" 
let histogram (data:string) = 
    data
    |> Seq.toList // convert string to list of char
    /// list of chars in string data

let demox = histogram data



type A1 = | Monday1 of Unit | Tuesday1 of Unit
type A2 = Monday2 | Tuesday2


[<EntryPoint>]
let main argv =
    print "Hello World from F#!"

    let str = "test print"
    print str

    print doubleList[3;10;5]
    
    Console.ReadKey() |> ignore // not needed running from Visual Studio
    0 // return an integer exit code
    