(*
--- Reduction, Specification and Variant ---
1. (Step-by-step evaluation)
product 3
-> 3 * product (3-1)
-> 3 * product 2
-> 3 * 2 * product (2-1)
-> 3 * 2 * product 1
-> 3 * 2 * 1
-> 6 * 1
-> 6

2. It computes the factorial.

3. (Specification)
product n
TYPE: int -> int
PRE: n >= 1
POST: n! (factorial of n), except 0! is not defined
EXAMPLES: product 3 = 6

4. (Variant)
VARIANT: n
*)

(*
--- Currying ---
*)

(* minus x y
   TYPE: int -> int -> int
   PRE: true
   POST: x - y
*)
val minus = fn x => fn y => x - y;

(*
- foo will get the value 1

- bar will be a function fn y => 5 - y

- Step-by-step:
minus 5 4
-> (fn x => fn y => x - y) 5 4
-> (fn y => 5 - y) 4
-> 5 - 4
-> 1
*)

(*
--- Types ---
*)

(* fun1 x
   TYPE: int -> int
   PRE: true
   POST: x
*)
fun fun1 x : int = x;

(* fun2 x y
   TYPE: int -> int -> int
   PRE: true
   POST: x + y
*)
fun fun2 x y : int = x + y;

(* fun3 x
   TYPE: int -> int * int
   PRE: true
   POST: returns a 2-element tuple with copies of x
*)
fun fun3 (x : int) = (x, x);

(* fun4 (x, y)
   TYPE: int * int -> int
   PRE: true
   POST: x + y
*)
fun fun4 (x : int, y : int) = x + y;

(* fun5 x y z
   TYPE: int -> real -> string -> string
   PRE: true
   POST: returns z if x and y are larger than 0, otherwise an empty string
*)
fun fun5 x y z = if x>0 andalso y>0.0 then z^"" else "";

(* fun6 (a, (b, c, d))
   TYPE: int * (string * string * int) -> int * string
   PRE: true
   POST: returns 2-element tuple with the sum of a and d and the concatenation of b and c
*)
fun fun6 (a : int, (b, c, d)) = (a + d, b ^ c);

(*
--- Least Common Multiple ---
See https://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm
*)

(* gcd (x, y)
   TYPE: int * int -> int
   PRE: true
   POST: returns the greatest common divisor of x and y
*)
(* VARIANT: x mod y *)
fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, a mod b);

(* lcm2 (x, y)
   TYPE: int * int -> int
   PRE: true
   POST: returns the least common multiple of x and y
*)
fun lcm2 a b = a*b div gcd (a, b);

(* lcm n
   TYPE: int -> int
   PRE: n >= 1
   POST: largest common multiple of 1,...,n
   EXAMPLES: lcm 10 = 2520
             lcm 0 = 0
*)
fun lcm n =
    let
        (* helper a
           TYPE: int -> int
           PRE: n >= 1
           POST: largest common multiple of 1,...,n*)
        (* VARIANT: a *)
        fun helper 1 = 1
          | helper a = lcm2 (helper (a-1)) a
    in
        if n < 1 then 0 else
        helper n
    end;

(* plus x y
   TYPE: int -> int -> int
   PRE: true
   POST: x + y
*)
fun plus x y = x + y;
