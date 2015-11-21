(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a *)
fun all_except_option (str, strlist) =
    case strlist of
        [] => NONE
      | x::xs' =>
            if same_string(x, str)
            then SOME xs'
            else case all_except_option(str, xs') of
                     NONE => NONE
                   | SOME lst => SOME (x::lst)

(* 1.b *)
fun get_substitutions1 (strlst_lst, str) =
    case strlst_lst of
        [] => []
      | x::xs' => case all_except_option (str, x) of
                      NONE => get_substitutions1 (xs', str)
                    | SOME subs => subs @ get_substitutions1 (xs', str)
                                         
(* 1.c *)
fun get_substitutions2 (strlst_lst, str) =
    let
        fun iter (strlst_lst, acc) =
            case strlst_lst of
                [] => acc
              | x::xs' => case all_except_option (str, x) of
                              NONE => iter (xs', acc)
                            | SOME subs => iter (xs', acc @ subs)
    in
        iter (strlst_lst, [])
    end

(* 1.d *)
fun similar_names (strlst_lst, full_name) =
    let
        val {first = f, last = l, middle = m} = full_name
        fun gen_full_names subs_names =
            case subs_names of
                [] => []
              | x::xs' => {first = x, last = l, middle = m} :: gen_full_names xs'
    in
        full_name :: (gen_full_names (get_substitutions2 (strlst_lst, f)))
    end
    
(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2.a *)
fun card_color c =
    case c of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

(* 2.b *)
fun card_value c =
    case c of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

(* 2.c *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs' => if x = c
                  then xs'
                  else x::remove_card(xs', c, e)

(* 2.d *)
(*
fun all_same_color cs =
    case cs of
        [] => true
      | first::second::rest => (card_color first) = (card_color second) andalso
                               (all_same_color (second::rest))
      | x::xs' => true
*)
fun all_same_color cs =
    case cs of
        [] => true
      | _::[] =>true
      | first::second::rest => (card_color first) = (card_color second) andalso
                               (all_same_color (second::rest))

(* 2.e *)
fun sum_cards cs =
    let fun iter (cs, acc) =
            case cs of
                [] => acc
              | x::xs' => iter (xs', acc + (card_value x))
    in
        iter (cs, 0)
    end

(* 2.f *)
fun score (cs, goal) =
    let
        val sum_of_cards = sum_cards cs
        val pre_score = if sum_of_cards > goal
                        then 3 * (sum_of_cards - goal)
                        else goal - sum_of_cards
    in
        if all_same_color cs
        then pre_score div 2
        else pre_score
    end
        
fun officiate (card_list, move_list, goal) =
    let
        fun game_run (card_list, move_list, held_cards) =
            case move_list of
                [] => held_cards
              | (Discard c)::move' =>
                game_run (card_list, move', remove_card(held_cards, c, IllegalMove))
              | Draw::move' =>
                case card_list of
                    [] => held_cards
                  | c::c'=> let val held_cards_tmp = c::held_cards
                            in if sum_cards held_cards_tmp > goal
                               then held_cards_tmp
                               else game_run(c', move', held_cards_tmp)
                            end
    in
        score (game_run (card_list, move_list, []), goal)
    end
