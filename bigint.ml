(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (* Indicate signs and place largest passed number on top *)
    (* Assume functions always possess carry or borrow, if none then set to 0 *)
    (* Move from low order to high order digits using tail recursion *)
    let rec cmp list1 list2 = match (list1, list2) with
        | list1, []                 -> 1
	| [], list2                 -> 0
	| car1::cdr1, car2::cdr2    ->
	  if car1 > car2
	  then 1
	  else if car2 > car1
	  then 0
	  else cmp cdr1 cdr2

    (* Handle all cases for addition *)
    (* Large numbers split into lists with single-digit elements *)
    (* Lists cannot support signed ints, so lists passed into add() as Bigints *)
    (* ( (car1 + car2 + carry) + 10) % 10 *)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    (* Basically mirrors original add' function in syntax and use *)
    (* ( (car1 - car2 - carry) + 10) % 10 *)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in  diff mod radix :: sub' cdr1 cdr2 (diff / radix)

    (* Function order MATTERS in ocaml *)
    (* sub needed by add to operate on signed integers, so it goes first *)
    let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if (sign1 = Pos && sign2 = Pos)
        then (
            if (cmp value1 value2) = 1
            then Bigint (sign1, sub' value1 value2 0)
            else Bigint (Neg, sub' value2 value1 0) )
        else if (sign1 = Neg && sign2 = Neg)
        then (
            if (cmp value1 value2) = 1
            then Bigint (sign1, add' value1 value2 0)
            else Bigint (Pos, sub' value2 value1 0) )
        else Bigint (sign1, add' value1 value2 0) 

    (* Modified original add to support signed ints *)
    (* Handle all case combinations for signed addition *)
    (* Calls made to sub' to fix negative addition cases *)
    let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = sign2
        then Bigint (sign1, add' value1 value2 0)
        else if (sign1 = Pos && sign2 = Neg)
	then (
	    if (cmp value1 value2) = 1
	    then Bigint (sign1, sub' value1 value2 0)
	    else Bigint (sign2, sub' value2 value1 0) )
	else if (sign1 = Neg && sign2 = Pos)
	then (
	    if (cmp value1 value2) = 1
	    then Bigint (sign1, sub' value1 value2 0)
	    else Bigint (sign2, sub' value2 value1 0) )
	else (
	    if (cmp value1 value2) = 1
	    then Bigint (sign1, sub' value1 value2 0)
	    else Bigint (sign2, sub' value2 value1 0) )

    (* To multiply, add correct elements of right column in each list *)
    let rec mul' list1 list2 =
        if (car list2) = 1
        then list1
        else (add' list1 (mul' list1 (sub' list2 [1] 0) ) 0)

    (* Handle all case combinations for signed multiplication *)
    let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = sign2
        then Bigint (Pos, mul' value1 value2)
        else Bigint (Neg, mul' value1 value2) 

    (* To divide, add correct elements of left column in each list *)
    (* Use accumulator method for tail recursion *)
    let rec div' list1 list2 sol =
        if (cmp list1 list2) = 0
        then (sol, list1)
        else (div' (sub' list1 list2 0) list2 (add' sol [1] 0) )

    (* Handle all case combinations for signed division *)
    (* Handle division by zero case specifically *)
    let div (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if (car value2) <> 0 then (
            if sign1 = sign2
            then Bigint (Pos, fst (div' value1 value2 [0]) )
            else Bigint (Neg, fst (div' value1 value2 [0]) ) )
        else (printf "Division by zero error\n"; Bigint (Pos, [0]) ) 

    (* Separate remainder function instead of returning it with division *)
    let rem (Bigint (sign1, list1)) (Bigint (sign2, list2)) = 
        if (car list2) <> 0 then (
            if sign1 = sign2
            then Bigint (sign1, snd (div' list1 list2 [0]) )
            else Bigint (Neg, snd (div' list1 list2 [0]) )
        )
        else (printf "Remainder by zero error\n"; Bigint (Pos, [0]) )

    (* Same logic as mul' except with a much deeper recursive call stack *)
    let rec pow' list1 list2 = 
        if (car list2) = 1
        then list1
        else (mul' list1 (pow' list1 (sub' list2 [1] 0) ) )
    
    (* Completely dependent on previous functions *)
    let pow (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign2 = Neg
        then (Bigint (Pos, []))
        else if sign1 = Pos
            then (Bigint (sign1, pow' value1 value2))
            else if rem (Bigint (Pos, value2)) (Bigint (Pos, [2])) = (Bigint (Pos, [1]) )
                then (Bigint (Neg, pow' value1 value2))
                else (Bigint (Pos, pow' value1 value2))

end
