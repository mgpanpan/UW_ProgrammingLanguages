use "funny_sum.sml";

val test_in1 = [I 1, I 2, I 3, I 4, I 5]
val test_in2 = [I 1, I 2, S "sml", S "racket", I 3, S "ruby"]
val out1 = funny_sum test_in1
val out2 = funny_sum test_in2
