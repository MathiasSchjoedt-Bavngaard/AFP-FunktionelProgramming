module Week1.Libary


//Declare a function h: float * float -> float, where h(x, y) = x2 + y2. Hint: Use
//the function System.Math.Sqrt.



let h (x:float, y:float) = sqrt(x*x + y*y)
let hTest = h(3.0, 4.0) = 5.0

//The sequence F0, F1, F2,... of Fibonacci numbers is defined by:
// F0 = 0
// F1 = 1
// Fn = Fn−1 + Fn−2
// Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13,....
// Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
// correspond to the three cases of the above definition.
// Give an evaluations for F4

let rec Fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> Fibonacci(n-1) + Fibonacci(n-2)
    
let FibonacciTest = Fibonacci 4 = 3
let FibonacciTest2 = Fibonacci 5 = 5

let FibonacciFirst10Test = [0;1;1;2;3;5;8;13;21;34] = [for i in 0..9 -> Fibonacci i]
  
  
let rec sum (m,n) =
    match (m,n) with
    | (m, 0) -> m
    | (m, n) -> sum(m, n-1) + (m+n)
    
    
let sumTest = sum(3, 4)
let sum12 = sum(1,3 )  


let rec pow(s:string, n:int) =
    match n with
    | 0 -> ""
    | _ -> s + pow(s, n-1)
    
    
let powTest = pow("a", 5) = "aaaaa"

let  isIthChar(s:string, i:int, c:char) = s[i] = c

let isIthCharTest = isIthChar("abc", 1, 'b') = true

// Declare the F# function occInString: string * char -> int where
// occInString(str, ch) = the number of occurrences of character ch
// in the string str.

let rec occInString(str: string, ch: char) =
    match str with
    | "" -> 0
    | _ -> occInString(str.Substring(1), ch) + (if str.[0] = ch then 1 else 0)
    
let occInStringListCoprehention (str: string, ch: char) =
    [for c in str do if c = ch then 1 else 0] |> List.sum
    
let occInStringSeq(str: string, ch: char) = 
    str |> Seq.filter (fun c ->  c = ch ) |> Seq.length
    


let occInStringTest = occInString("abckajsgdjaaslkjdaljsdgf", 'a') = 5
let occInStringTest2 = occInStringListCoprehention("abckajsgdjaaslkjdaljsdgf", 'a') = 5

let occInStringTest3 = occInStringSeq("abckajsgdjaaslkjdaljsdgf", 'a') = 5


let rec bin (n: int, k: int) =
    if n = k || k = 0
        then 1
    else
        bin(n-1, k-1) + bin(n-1, k)
 
let binTest = bin(5, 2) = 10
let binTest2 = bin(2, 1) = 2
let binTest3 = bin(4, 2) = 6


//4.1  Declare function upto: int -> int list such that upto n = [1; 2; ... ; n]
let upto n = [1..n]

let uptoTest = upto 5 = [1;2;3;4;5]
let doubleUpto n = [1..n] |> List.map (fun x -> x * 2)
//Declare function downto1: int -> int list such that the value of downto1 n is the list
//[n; n − 1; ... ; 1].

let downto1 = List.rev << upto



let rec altsum = function
    | [] -> 0
    | [x] -> x
    | x0::x1::xs -> x0 - x1 + altsum xs;;


let dope = altsum [2; -1; 3];;

//first :: rest aproach
let rec newAltSum = function 
    | [] -> 0
    | first::rest -> first + newAltSum rest
    
let newAltSumTest = newAltSum [2; -1; 3]
let newAltSumTest2 = newAltSum [2; -1; 3; 4; 5; 6; 7; 8; 9; 10]  

    
