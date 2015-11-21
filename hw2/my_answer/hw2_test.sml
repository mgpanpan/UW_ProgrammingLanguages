(* 1.a *)
val lst1 = ["hello", "world", "sml", "~~~"];
val lst2 = [];
all_except_option("hello", lst1);
all_except_option("hello", lst2);
all_except_option("world", lst1);
all_except_option("sml", lst1);
all_except_option("~~~", lst1);
all_except_option("racket", lst1);
all_except_option("hello", ["hello"]);

(* 1.b *)
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                  ["Freddie","Fred","F"]], "Fred");
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
                    ["Geoff","Jeff","Jeffrey"]], "Jeff");
(* 1.c *)
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],
                  ["Freddie","Fred","F"]], "Fred");
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],
                    ["Geoff","Jeff","Jeffrey"]], "Jeff");

(* 1.d *)
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],
               ["Freddie","Fred","F"]],
             {first="Fred", middle="W", last="Smith"});


(* 2.a *)
val card1 = (Clubs, Jack);
val card2 = (Diamonds, Queen);
val card3 = (Hearts, King);
val card4 = (Spades, Num 5);
val card5 = (Clubs, Ace);
val card6 = (Hearts, Num 3);
val card7 = (Spades, Num 2);
val card8 = (Diamonds, Num 4);
val card9 = (Spades, Num 6);
val card10 = (Hearts, Num 7);
val card11 = (Clubs, Num 8);
val card12 = (Spades, Num 9);

card_color card1;
card_color card2;
card_color card3;
card_color card4;

(* 2.b *)
card_value card1;
card_value card2;
card_value card3;
card_value card4;
card_value card5;
card_value card6;

(* 2.c *)
val cards1 = [card1, card2, card3, card4, card5, card6];
remove_card(cards1, card2, IllegalMove);
remove_card(cards1, card1, IllegalMove);
remove_card(cards1, card6, IllegalMove);
(* next function call will raise an exception *)
(* remove_card(cards1, (Diamonds, Num 6), IllegalMove); *)

(* 2.d *)
all_same_color(cards1);
all_same_color([card1, card2]);
all_same_color([card1, card3]);
all_same_color([card1, card4]);
all_same_color([card1, card2, card3]);
all_same_color([card2, card3]);

(* 2.e *)
sum_cards(cards1);
sum_cards([card1, card2]);
sum_cards([card5, card6]);

(* 2.f *)
score(cards1, 10);
score(cards1, 100);
score([card1, card4], 10);
score([card2, card5], 10);

(* 2.g *)
val test_card_list = [card1,card6,card11,card4,card9,card2,card7,card12,card5,
                     card10];
val move_list1 = [Draw, Draw, Draw, Draw, Draw];
val move_list2 = [Draw, Draw, Draw, Draw, Discard card1,
                 Draw, Discard card6, Draw];
val move_list3 = [Draw, Draw, Draw, Draw];

officiate(test_card_list, move_list1, 32);
officiate(test_card_list, move_list2, 32);
officiate([card1, card6, card11], move_list1, 25);
officiate([card1, card4, card5], move_list3, 25); (* same color *)
officiate([card1, card4, card5], move_list3, 27); (* same color *)
officiate([card1, card4, card5], move_list3, 28); (* same color *)
