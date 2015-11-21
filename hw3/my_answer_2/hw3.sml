(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer
              
(* 1 *)
val only_capitals =
    List.filter (fn str => Char.isUpper(String.sub(str, 0)))

(* 2 *)
val longest_string1 =
    foldl (fn (str, acc) => if String.size str > String.size acc
                             then str else acc) ""

(* 3 *)
val longest_string2 =
    foldl (fn (str, acc) => if String.size str < String.size acc
                            then acc else str) ""
          
(* 4 *)
fun longest_string_helper compare_fun strlst = 
    foldl (fn (str, acc) => if compare_fun(String.size str, String.size acc)
                            then str else acc)
          "" strlst

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
(* val implode : char list -> string *)
(* val explode : string -> char list *)
val rev_string = implode o List.rev o explode
(* String.implode, String.explode *)
                                          
(* 7 *)
fun first_answer check_fun lst =
    case lst of
        [] => raise NoAnswer
      | x::xs' => case check_fun x of
                      SOME v => v
                    | NONE => first_answer check_fun xs'

(* 8 *)
fun all_answers check_fun lst =
    let fun iter (lst, acc) =
            case lst of
                [] => SOME acc
              | x::xs' => case check_fun x of
                              SOME vs => iter (xs', acc @ vs)
                            | NONE => NONE
    in
        iter (lst, [])
    end

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu
                                            
fun g f1 f2 p =
    let 
	    val r = g f1 f2 
    in
	    case p of
	        Wildcard          => f1 ()
	      | Variable x        => f2 x
	      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
    end

(* 9.a *)
val count_wildcards = g (fn _ => 1) (fn x => 0)
(* 9.b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size
(* 9.c *)
fun count_some_var (str, p) =
    g (fn _ => 0) (fn x => if x = str then 1 else 0) p

(* 10 *)
(* can use val binding and the combiner o *)
fun check_pat p =
    let
        fun varnames_in_pat p =
    	    case p of
	            Variable x        => [x]
	          | TupleP ps         =>
                List.foldl (fn (p,acc) => (varnames_in_pat p) @ acc) [] ps
	          | ConstructorP(_,p) => varnames_in_pat p
	          | _                 => []
        fun whether_repeats strlst =
            case strlst of
                [] => true
              | x::xs' => not(List.exists (fn y => x = y) xs') andalso
                          (whether_repeats xs')
    in
        whether_repeats (varnames_in_pat p)
    end

(* 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (valu, Variable str) => SOME [(str, valu)]
      | (Unit, UnitP) => SOME []
      | (Const num, ConstP numP) => if num = numP then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(vstr, v), ConstructorP(pstr,p)) =>
        if vstr = pstr then match(v, p) else NONE
      | _ => NONE
    
(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE
    
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
