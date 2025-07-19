(**Generic number type to convert between int and floats.**)
type number =
    | Int of int
    | Float of float
;;

(**Print a number value to standard output.**)
let print_number n  =
match n with
| Int n -> print_int n
| Float n -> print_float n
;;

let add (a: number) (b: number) : number = 
    match (a, b) with
    | Int a, Int b -> Int(a + b)
    | Int a, Float b -> Float(float_of_int a +. b) 
    | Float a, Int b -> Float(a +. float_of_int b)
    | Float a, Float b -> Float(a +. b)
;;
let sub (a: number) (b: number) : number =
    match (a, b) with
    | Int a, Int b -> Int(a - b)
    | Int a, Float b -> Float(float_of_int a -. b) 
    | Float a, Int b -> Float(a -. float_of_int b)
    | Float a, Float b -> Float(a -. b)
;;
let mul (a: number) (b: number) : number = 
    match (a, b) with
    | Int a, Int b -> Int(a * b)
    | Int a, Float b -> Float(float_of_int a *. b) 
    | Float a, Int b -> Float(a *. float_of_int b)
    | Float a, Float b -> Float(a *. b)
;;

let div (a: number) (b: number) : number =
    match (a, b) with
    | Int a, Int b -> Int(a / b)
    | Int a, Float b -> Float(float_of_int a /. b) 
    | Float a, Int b -> Float(a /. float_of_int b)
    | Float a, Float b -> Float(a /. b)
;;


print_number (add (Int 1) (Int 2));;
print_newline ();;

print_number (add (Int 1) (Float 2.0));;
print_newline ();;

print_number (sub (Int 1) (Int 2));;
print_newline ();;

print_number (sub (Float 1.0) (Int 2));;
print_newline ();;

print_number (mul (Int 1) (Int 2));;
print_newline ();;

print_number (mul (Float 1.0) (Float 2.0));;
print_newline ();;

print_number (div (Int 1) (Int 2));;
print_newline ();;

print_number (div (Float 1.0) (Int 2));;
print_newline ();;
