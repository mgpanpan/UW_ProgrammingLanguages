exception Error of string

datatype exp = Const of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval_exp e =
    let fun get_int e =
            case e of
                Const i => i
              | _ => raise (Error "expected Const result")
    in
        case e of
            Const _ => e
          | Negate e2 => Const (~ (get_int(eval_exp e2)))
          | Add(e1, e2) => Const (get_int(eval_exp e1) + get_int(eval_exp e2))
          | Multiply(e1, e2) => Const (get_int(eval_exp e1) * get_int(eval_exp e2))
    end
        
