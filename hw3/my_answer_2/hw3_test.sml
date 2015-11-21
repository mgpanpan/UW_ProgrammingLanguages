use "hw3.sml";

(* 1 *)
val test1_1 = ["Hello", "programming", "languages",
              "SML", "Racket", "Ruby"]
val test1_2 = ["Hello", "programming", "Languages",
              "SML", "Racket", "ruby"]
val test1_1out = only_capitals test1_1 = ["Hello","SML","Racket","Ruby"]
val test1_2out = only_capitals test1_2 = ["Hello","Languages","SML","Racket"]

(* 2, 3 *)
val test2_1 = test1_1
val test2_2 = ["Hello", "programming", "languages",
               "SML", "Racket", "Ruby", "programminl"]
val test2_1out = longest_string1 test2_1 = "programming"
val test2_2out = longest_string1 test2_2 = "programming"
val test3_1 = test2_1
val test3_2 = test2_2
val test3_1out = longest_string2 test3_1 = "programming"
val test3_2out = longest_string2 test3_2 = "programminl"

(* 4 *)
val test4_1 = test2_1
val test4_2 = test2_2
val test4_1out = longest_string3 test4_1 = longest_string1 test4_1
val test4_2out = longest_string3 test4_2 = longest_string1 test4_2
val test4_1out2 = longest_string4 test4_1 = longest_string2 test4_1
val test4_2out2 = longest_string4 test4_2 = longest_string2 test4_2
                                               
(* 5 *)
val test5_1 = test2_1
val test5_2 = test2_2
val test5_1out = longest_capitalized test5_1 = "Racket"
val test5_2out = longest_capitalized test5_2 = "Racket"

(* 6 *)
val test6_1 = rev_string "Hello" = "olleH"
val test6_2 = rev_string "racket" = "tekcar"

(* 7 *)
val test7_1 = [1, 1, 2, 3, 0, 0, 10, 9, 8]
val test7_1out = first_answer (fn x => if x=3 then SOME x else NONE) test7_1
                 = 3
(* raise exception NoAnswer *)
(* val test7_1out2 = first_answer (fn x => if x=11 then SOME x else NONE) test7_1 *)

(* 8 *)
val test8_1 = [1, 1, 2, 3, 0, 0, 10, 9, 8]
val test8_1out1 = all_answers (fn x => if x<>0 then SOME [x,x] else NONE) test8_1
                  = NONE
val test8_1out2 = all_answers (fn x => if x>10 then SOME [x,x] else NONE) test8_1
                  = NONE
val test8_1out3 = all_answers (fn x => if x<=10 then SOME [x,x] else NONE) test8_1
                  = SOME [1,1,1,1,2,2,3,3,0,0,0,0,10,10,9,9,8,8]
val test8_1out4 = all_answers (fn x => if x<=10 then SOME [x,x] else NONE) []
                  = SOME []

(* 9 *)
val pattern_test = TupleP [ConstP 10, Variable "a", Variable "emacs",
                           UnitP, Wildcard,
                           TupleP [ConstructorP ("Cons", ConstP 20),
                                   Variable "emacs", Wildcard],
                           Wildcard]
val test9_a = count_wildcards pattern_test = 3
val test9_b = count_wild_and_variable_lengths pattern_test = 14
val test9_c = count_some_var("emacs", pattern_test) = 2

(* 10 *)
(* val test10_1 = (whether_repeats o varnames_in_pat) pattern_test; *)
(* val test10_tmp = varnames_in_pat pattern_test *)
val test10_1 = check_pat pattern_test = false

(* 11 *)
val pattern_test = TupleP [ConstP 10, Variable "a", Variable "emacs",
                           UnitP, Wildcard,
                           TupleP [ConstructorP ("Cons", ConstP 20),
                                   Variable "emacs", Wildcard],
                           Wildcard]
val value_test = Tuple [Const 10, Const 20, Const 60,
                        Unit, Unit,
                        Tuple [Constructor ("Cons", Const 20),
                               Tuple [Const 10, Const 20], Const 80],
                        Tuple [Const 1000]]
val test11_1 = match (value_test, pattern_test)
               = SOME [("a", Const 20), ("emacs", Const 60),
                      ("emacs", Tuple [Const 10, Const 20])]

(* 12 *)
val p_test1 = [ConstP 10, ConstP 20, ConstP 60]
val p_test1out = first_match (Const 20) p_test1
                 = SOME []
                 
val p_test2 = [Variable "emacs", Variable "sml", Variable "racket"]
val p_test2out = first_match (Const 20) p_test2
                 = SOME [("emacs", Const 20)]
                        
val p_test3 = [TupleP [Variable "emacs", Variable "sml", Variable "racket"]]
val p_test3out = first_match (Tuple [Const 10, Const 20, Const 30]) p_test3
                 = SOME [("emacs",Const 10),("sml",Const 20),("racket",Const 30)]
                        
val p_test4 = [ConstP 10, ConstP 20, ConstP 60]
val p_test4out = first_match (Const 30) p_test1
                 = NONE

