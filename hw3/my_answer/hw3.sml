(* 1 *)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(* 2 *)
(* -------------------------------------------------------------------
   foldl f init [x1, x2, ..., xn]
   returns f(xn, ..., f(x2, f(x1, init))...)
   or init if the list is empty
   ---------------------------------------------------------------- *)
fun longest_string1 xs =
    let fun longest_of_two (x, y) =
            if String.size x <= String.size y
            then y
            else x
    in
        foldl longest_of_two "" xs
    end

(* 3 *)
fun longest_string2 xs =
    let fun longest_of_two (x, y) =
            if String.size x < String.size y
            then y
            else x
    in
        foldl longest_of_two "" xs
    end

(* 4 *)
fun longest_string_helper predicate xs =
    foldl (fn (x,y) => if predicate(String.size(x),String.size(y))
                       then y
                       else x)
          "" xs

fun longest_string3 xs =
    longest_string_helper (fn (x,y) => x<=y) xs

fun longest_string4 xs =
    longest_string_helper (fn (x,y) => x<y) xs
                          
(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
(* val implode : char list -> string *)
(* val explode : string -> char list *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
exception NoAnswer

(*
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' =>
        let val y = f x
        in if isSome y
           then valOf y
           else first_answer f xs'
        end
*)

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' =>
        case f x of
            NONE => first_answer f xs'
          | SOME v => v

(* 8 *)
fun all_answers f xs =
    let fun iter (acc, xs) =
            case xs of
                [] => SOME acc
              | x::xs' =>
                case f x of
                    NONE => NONE
                  | SOME v => iter(acc @ v, xs')
    in iter([], xs) end

(* provided type definitions *)
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

(* provided function g *)
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

(* 9 *)
fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p

fun count_some_var (str, p) =
    g (fn () => 0) (fn x => if x = str
                            then 1 else 0) p

(* 10 *)
val check_pat =
    let 
        fun list_of_variables p =
            case p of
	            Wildcard   => []
	          | Variable x => [x]
	          | TupleP ps  => List.foldl (fn (x,y) => y @ list_of_variables x ) [] ps
	          | ConstructorP(_,p) => list_of_variables p
	          | _                 => []
        fun whether_repeats listp =
            case listp of
                [] => true
              | first::rest => not(List.exists (fn x => x=first) rest) andalso
                               whether_repeats rest
    in 
        whether_repeats o list_of_variables
    end

(* 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v ,Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x=y then SOME [] else NONE
      | (Tuple vlist, TupleP plist) =>
        if length(vlist) = length(plist)
        then all_answers match (ListPair.zip(vlist, plist))
        else NONE
      | (Constructor(vstr, v1), ConstructorP(pstr, p1)) =>
        if pstr = vstr then match(v1,p1) else NONE
      | _ => NONE

(* 12 *)
fun first_match v plist =
    SOME (first_answer (fn x => match(v, x)) plist)
         handle NoAnswer => NONE

(* Challenge problem *)
(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

