use "expressions_small_languages.sml";

val exp1 = Add(Add(Int 5, Int 7), Negate(Int 3))

val exp1_eval = eval exp1

val exp1_str = toString exp1

val exp1_zerodetect = hasZero exp1
