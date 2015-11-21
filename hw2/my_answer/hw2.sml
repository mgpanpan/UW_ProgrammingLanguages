fun same_string(s1 : string, s2 : string) =
    s1 = s2
(* put your solutions for problem 1 here *)
(* you may assume that Num is always used with values 2, 3, ..., 9
though it will not really come up *)

(* 1.a *)
fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
      | x::xs' => if same_string(x, str)
                  then SOME xs'
                  else let val afterwards = all_except_option(str, xs')
                       in
                           if isSome afterwards
                           then SOME (x :: valOf(afterwards))
                           else NONE
                       end

(* 1.b *)
fun get_substitutions1(substitutions, str) =
    case substitutions of
        [] => []
      | x::xs' => let val tmp = all_except_option(str, x)
                  in
                      if isSome tmp
                      then valOf(tmp) @ get_substitutions1(xs', str)
                      else get_substitutions1(xs', str)
                  end

(* 1.c *)
fun get_substitutions2(substitutions, str) =
    let fun iter(substitutions, acc) =
            case substitutions of
                [] => acc
              | x::xs' =>
                let val tmp = all_except_option(str, x)
                in
                    if isSome tmp
                    then iter(xs', acc @ valOf(tmp))
                    else iter(xs', acc)
                end
    in iter(substitutions, [])
    end

(* 1.d *)
fun similar_names(substitutions, full_name) =
    let val {first=name_first, middle=name_middle, last=name_last} = full_name
        val first_subs = get_substitutions2(substitutions, name_first)
        fun iter(subs, acc) =
            case subs of
                [] => full_name :: acc
              | x::xs' =>
                iter(xs',
                     {first=x,middle=name_middle,last=name_last}::acc)
    in 
        iter(first_subs, [])
    end
        
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw
exception IllegalMove

(* 2.a *)
fun card_color card =
    case card of
        (Spades, _) => Black
      | (Clubs,  _) => Black
      | _ => Red

(* 2.b *)
fun card_value card =
    case card of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

(* 2.c *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs' =>
        if x = c
        then xs'
        else x::remove_card(xs', c, e)

(* 2.d *)
fun all_same_color cards =
    let fun same_color_between2cards(card1, card2) =
            if card_color(card1) = card_color(card2)
            then true
            else false
    in
        case cards of
            [] => true
          | _::[] => true
          | first::(second::rest) =>
            same_color_between2cards(first, second) andalso
            all_same_color(second::rest)
    end

(* 2.e *)
fun sum_cards cards =
    let fun iter (cards, acc) =
            case cards of
                [] => acc
              | x::xs' => iter(xs', card_value(x)+acc)
    in
        iter(cards, 0)
    end

(* 2.f *)
fun score (held_cards, goal) =
    let val held_cards_sum = sum_cards(held_cards)
        val preliminary_score = if held_cards_sum > goal
                                then 3 * (held_cards_sum - goal)
                                else (goal - held_cards_sum)
    in
        if all_same_color(held_cards)
        then preliminary_score div 2
        else preliminary_score
    end

(* 2.g *)
fun officiate (card_list, move_list, goal) =
    let fun process(card_list, held_list, move_list) =
            case move_list of
                [] => score(held_list, goal)
              | current_move::rest_move =>
                case (current_move, card_list) of
                    (Discard card, _) =>
                    process(card_list,
                            remove_card(held_list, card, IllegalMove),
                            rest_move)
                  | (Draw, []) =>
                    score(held_list, goal)
                  | (Draw, next_card::rest_card) =>
                    let val new_held_list = next_card :: held_list
                    in if sum_cards(new_held_list) > goal
                       then score(new_held_list, goal)
                       else process(rest_card, new_held_list, rest_move)
                    end
    in
        process(card_list, [], move_list)
    end

(* challenge problems *)
(* 3.a *)
(* fun score_challenge = *)

(* 3.b *)
