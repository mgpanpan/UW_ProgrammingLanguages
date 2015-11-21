use "simple_exp.sml";

val test_exp = Multiply (Negate (Add (Const 2, Const 2)),
                         (Const 7))
val test_val = eval_exp test_exp
