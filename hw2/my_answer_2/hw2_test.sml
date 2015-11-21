use "hw2.sml";

(* 1.a *)
val lst1 = ["hello", "world", "sml", "~~~"]
val lst2 = []
val test1 = all_except_option("hello", lst1) = SOME ["world", "sml", "~~~"]
val test2 = all_except_option("hello", lst2) = NONE
val test3 = all_except_option("world", lst1) = SOME ["hello", "sml", "~~~"]
val test4 = all_except_option("sml", lst1) = SOME ["hello", "world", "~~~"]
val test5 = all_except_option("~~~", lst1) = SOME ["hello", "world", "sml"]
val test6 = all_except_option("racket", ["hello", "world"]) = NONE
val test7 = all_except_option("hello", ["hello"]) = SOME []

(* 1.b *)
val test8 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                ["Freddie","Fred","F"]], "Fred")
                              = ["Fredrick","Freddie","F"]
val test9 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
                                ["Geoff","Jeff","Jeffrey"]], "Jeff")
                              = ["Jeffrey","Geoff","Jeffrey"]

(* 1.c *)
val test10 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],
                                 ["Freddie","Fred","F"]], "Fred")
             = ["Fredrick","Freddie","F"]
val test11 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],
                                 ["Geoff","Jeff","Jeffrey"]], "Jeff")
             = ["Jeffrey","Geoff","Jeffrey"];
                                    
(* 1.d *)
val test12 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],
               ["Freddie","Fred","F"]],
              {first="Fred", middle="W", last="Smith"})
             = [{first="Fred",last="Smith",middle="W"},
                {first="Fredrick",last="Smith",middle="W"},
                {first="Freddie",last="Smith",middle="W"},
                {first="F",last="Smith",middle="W"}]

(* 2.a *)
val card1 = (Clubs, Jack);     (* Black *)
val card2 = (Diamonds, Queen); (* Red *)
val card3 = (Hearts, King);    (* Red *)
val card4 = (Spades, Num 5);   (* Black*)
val card5 = (Clubs, Ace);
val card6 = (Hearts, Num 3);
val card7 = (Spades, Num 2);
val card8 = (Diamonds, Num 4);
val card9 = (Spades, Num 6);
val card10 = (Hearts, Num 7);
val card11 = (Clubs, Num 8);
val card12 = (Spades, Num 9);

val test13 = card_color card1 = Black
val test14 = card_color card2 = Red
val test15 = card_color card3 = Red
val test16 = card_color card4 = Black
                                    
(* 2.b *)
val test17 = card_value card1 = 10
val test18 = card_value card2 = 10
val test19 = card_value card3 = 10
val test20 = card_value card4 = 5
val test21 = card_value card5 = 11
val test22 = card_value card6 = 3

(* 2.c *)                                    
val cards1 = [card1, card2, card3, card4, card5, card6];
val test23 = remove_card(cards1, card2, IllegalMove) = [card1, card3, card4, card5, card6]
val test24 = remove_card(cards1, card1, IllegalMove) = [card2, card3, card4, card5, card6]
val test25 = remove_card(cards1, card6, IllegalMove) = [card1, card2, card3, card4, card5]
(* next function call will raise an exception *)
(* remove_card(cards1, (Diamonds, Num 6), IllegalMove); *)

(* 2.d *)
val test26 = all_same_color(cards1) = false
val test27 = all_same_color([card1, card2]) = false
val test28 = all_same_color([card1, card3]) = false
val test29 = all_same_color([card1, card4]) = true
val test30 = all_same_color([card1, card2, card3]) = false
val test31 = all_same_color([card2, card3]) = true

(* 2.e *)
val test32 = sum_cards(cards1) = 49
val test33 = sum_cards([card1, card2]) = 20
val test34 = sum_cards([card5, card6]) = 14

(* 2.f *)
val test35 = score(cards1, 10) = 117
val test36 = score(cards1, 100) = 51
val test37 = score([card1, card4], 10) = 7
val test38 = score([card2, card5], 10) = 33

(* 2.g *)
val test_card_list = [card1,card6,card11,card4,card9,card2,card7,card12,card5,
                     card10]
val move_list1 = [Draw, Draw, Draw, Draw, Draw]
val move_list2 = [Draw, Draw, Draw, Draw, Discard card1,
                 Draw, Discard card6, Draw]
val move_list3 = [Draw, Draw, Draw, Draw]

(* 0 *)                     
val test39 = officiate(test_card_list, move_list1, 32) = 0
(* 3 *)
val test40 = officiate(test_card_list, move_list2, 32) = 3
(* 4 *)
val test41 = officiate([card1, card6, card11], move_list1, 25) = 4
(* (26 - 25) * 3 / 2 *) (* same color *)
val test42 = officiate([card1, card4, card5], move_list3, 25) = 1
(* (27 - 26) / 2*) (* same color *)
val test43 = officiate([card1, card4, card5], move_list3, 27) = 0 
(* (28 - 26) / 2*) (* same color *)
val test44 = officiate([card1, card4, card5], move_list3, 28) = 1
                                             
