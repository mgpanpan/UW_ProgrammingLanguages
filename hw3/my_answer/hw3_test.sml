use "hw3.sml";

(* 1 *)
val test1_1 = ["Hello", "programming", "languages",
              "SML", "Racket", "Ruby"];
val test1_2 = ["Hello", "programming", "Languages",
              "SML", "Racket", "ruby"];
val test1_1out = only_capitals test1_1;
val test1_2out = only_capitals test1_2;

(* 2, 3 *)
val test2_1 = test1_1;
val test2_2 = ["Hello", "programming", "languages",
               "SML", "Racket", "Ruby", "programminl"];
val test2_1out = longest_string1 test2_1;
val test2_2out = longest_string1 test2_2;
val test3_1 = test2_1;
val test3_2 = test2_2;
val test3_1out = longest_string2 test3_1;
val test3_2out = longest_string2 test3_2;

(* 4 *)
val test4_1 = test2_1;
val test4_2 = test2_2;
val test4_1out = longest_string3 test4_1;
val test4_2out = longest_string3 test4_2;
val test4_1out2 = longest_string4 test4_1;
val test4_2out2 = longest_string4 test4_2;

(* 5 *)
val test5_1 = test2_1;
val test5_2 = test2_2;
val test5_1out = longest_capitalized test5_1;
val test5_2out = longest_capitalized test5_2;

(* 6 *)
val test6_1 = rev_string "Hello"
val test6_2 = rev_string "racket"

(* 7 *)
val test7_1 = [1, 1, 2, 3, 0, 0, 10, 9, 8];
val test7_1out = first_answer (fn x => if x=3 then SOME x else NONE)
                              test7_1;

(* 8 *)
val test8_1 = [1, 1, 2, 3, 0, 0, 10, 9, 8];
val test8_1out1 = all_answers (fn x => if x<>0 then SOME [x,x] else NONE)
                              test8_1;
val test8_1out2 = all_answers (fn x => if x>10 then SOME [x,x] else NONE)
                              test8_1;
val test8_1out3 = all_answers (fn x => if x<=10 then SOME [x,x] else NONE)
                              test8_1;
val test8_1out4 = all_answers (fn x => if x<=10 then SOME [x,x] else NONE)
                              [];

(* 9 *)
val pattern_test = TupleP [ConstP 10, Variable "a", Variable "emacs",
                           UnitP, Wildcard,
                           TupleP [ConstructorP ("Cons", ConstP 20),
                                   Variable "emacs", Wildcard],
                           Wildcard];
val test_a = count_wildcards pattern_test;
val test_b = count_wild_and_variable_lengths pattern_test;
val test_c = count_some_var("emacs", pattern_test);

(* 10 *)
(* val test10_1 = (whether_repeats o list_of_variables) pattern_test; *)
val test10_1 = check_pat pattern_test;

(* 11 *)
val pattern_test = TupleP [ConstP 10, Variable "a", Variable "emacs",
                           UnitP, Wildcard,
                           TupleP [ConstructorP ("Cons", ConstP 20),
                                   Variable "emacs", Wildcard],
                           Wildcard];
val value_test = Tuple [Const 10, Const 20, Const 60,
                        Unit, Unit,
                        Tuple [Constructor ("Cons", Const 20),
                               Const 90, Const 80],
                        Tuple [Const 1000]];
val test11_1 = match (value_test, pattern_test);

(* 12 *)
val p_test1 = [ConstP 10, ConstP 20, ConstP 60];
val p_test1out = first_match (Const 20) p_test1;
val p_test2 = [Variable "emacs", Variable "sml", Variable "racket"];
val p_test2out = first_match (Const 20) p_test2;
val p_test3 = [TupleP [Variable "emacs", Variable "sml", Variable "racket"]];
val p_test3out = first_match (Tuple [Const 10, Const 20, Const 30]) p_test3
val p_test4 = [ConstP 10, ConstP 20, ConstP 60];
val p_test4out = first_match (Const 30) p_test1;
