fun same_string(s1 : string, s2 : string) =
    s1 = s2

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* Problem 1 *)
fun all_except_option (s, sl) =
  case sl of 
       [] => NONE
     | x::xs => case all_except_option(s, xs) of
                  NONE => if same_string(s, x) then SOME xs else NONE
                | SOME y => if same_string (s, x) then SOME y else SOME (x::y)

(* Problem 2 *)
fun get_substitutions1 (substitutions, s) =
  case substitutions of 
    [] => []
  | (xs::xss) => case all_except_option (s, xs) of
                      NONE => get_substitutions1 (xss, s)  
                    | SOME y => y @ get_substitutions1 (xss, s)


(* Problem 3 *)
fun get_substitutions2 (substitutions, s) =
  let 
    fun helper (xss, s, acc) =
      case xss of 
          [] => acc
        | (xs::tl) => case all_except_option (s, xs) of 
                          NONE => helper (tl, s, acc)
                        | SOME y => helper (tl, s, acc @ y)
  in 
    helper(substitutions, s, [])
  end


(* Problem 4 *)
fun similar_names (substitutions, {first=f, middle=m, last=l}) =
  let
    fun map (xs) =
      case xs of 
           [] => []
         | (x::tl) => {first=x, middle=m, last=l} :: map tl
  in
    {first=f, middle=m, last=l} :: map (get_substitutions2 (substitutions, f))
  end
   

(* Problem 5 *)
fun card_color ((suit, rank) : card) =
  case suit of
       Diamonds => Red
     | Hearts => Red
     | Clubs => Black
     | Spades => Black


(* Problem 6 *)
fun card_value ((suit, rank) : card) =
  case rank of
       Num n => n
     | Ace => 11
     | _ => 10


(* Problem 7 *)
fun remove_card (cs, c, e) =
  case cs of  
       (x::tl) => if x = c then tl else x::remove_card(tl, c, e)
     | [] => raise e


(* Problem 8 *)
fun all_same_color (cs) =
  case cs of
       [] => true
     | x::[] => true
     | x::y::ys => card_color x = card_color y andalso all_same_color (y::ys)


(* Problem 9 *)
fun sum_cards (cs) =
  let fun helper (cs, acc) =
    case cs of
         [] => acc
       | c::tl => helper (tl, acc + card_value c)
  in
    helper (cs, 0)
  end


(* Problem 10 *)
fun score (cs, goal) =
  let
    val sum = sum_cards(cs)
    val prelim_score = if sum > goal then 3*(sum - goal) else goal - sum
    val final_score = if all_same_color (cs)
                      then prelim_score div 2
                      else prelim_score
  in
    final_score
  end


(* Problem 11 *)
(* I decided to store all states in the helper to avoid recalculation *)
(* For example, the sum for the hand is being updated for each move rather than *)
(* recalculating it from scratch. *)
fun officiate (cs, mvs, goal) =
  let 
    fun evaluate_game (cs, mvs, hand, sum_hand, e: exn) =
      if sum_hand > goal then score (hand, goal)
      else
      case mvs of 
          [] => score (hand, goal)
        | mv::mtl => case (mv, cs) of 
                         (Draw, []) => score (hand, goal) 
                       | (Draw, c::ctl) => 
                           evaluate_game (ctl, mtl, c::hand, sum_hand + card_value c, e)
                       | (Discard hc, _) =>  
                           evaluate_game (cs, mtl, remove_card (hand, hc, e), sum_hand - card_value hc, e) 
  in
    evaluate_game (cs, mvs, [], 0, IllegalMove)
  end
