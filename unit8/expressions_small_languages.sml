exception BadResult of string

datatype exp =
         Int of int
       | Negate of exp
       | Add of exp * exp

fun eval e =
    case e of
        Int _ => e
      | Negate e1 => (case eval e1 of
                         Int x => Int (~x)
                       | _ => raise BadResult "non-int in negation")
      | Add(e1, e2) => (case (eval e1, eval e2) of
                            (Int x1, Int x2) => Int (x1+x2)
                          | _ => raise BadResult "non-int in addition")

fun toString e =
    case e of
        Int x => Int.toString x
      | Negate e1 => "-(" ^ (toString e1) ^ ")"
      | Add(e1, e2) => "(" ^ (toString e1) ^ " + " ^ (toString e2) ^ ")"

fun hasZero e =
    case e of
        Int x => x = 0
      | Negate e1 => hasZero e1
      | Add(e1, e2) => (hasZero e1) orelse (hasZero e2)
