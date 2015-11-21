use "callbacks.sml";

val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test1 = onEvent 10
val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test2 = onEvent 4
val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test3 = onEvent 11
val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test4 = onEvent 23
val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test5 = onEvent 30
val _ = print ((Int.toString (!timesPressed)) ^ "\n")
val test6 = onEvent 4
val _ = print ((Int.toString (!timesPressed)) ^ "\n")

