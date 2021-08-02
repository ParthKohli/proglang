(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* Problem 1 *)
val only_capitals =
  List.filter (fn x => Char.isUpper (String.sub (x, 0)))


(* Problem 2 *)
val longest_string1 =
  foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) ""
 

(* Problem 3 *)
val longest_string2 =
  foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) ""


(* Problem 4 *)
fun longest_string_helper f = 
  foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b) 
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)


(* Problem 5 *)
val longest_capitalized =
  longest_string1 o only_capitals 


(* Problem 6 *)
val rev_string = 
  implode o rev o explode


(* Problem 7 *)
fun first_answer f xs = 
  case xs of
       [] => raise NoAnswer
     | x::tl => case f x of 
                    NONE => first_answer f tl 
                  | SOME y => y


(* Problem 8 *)
fun all_answers f xs =
  let 
    fun all_answers_helper f xs acc =
      case xs of 
        [] => SOME acc
      | x::tl => case (f x) of 
                      NONE => NONE
                    | SOME y => all_answers_helper f tl (acc@y) 
  in
    all_answers_helper f xs []
  end
           


(* Problem 9 *)
val count_wildcards = g (fn () => 1) (fn x => 0) 

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn x => (if s = x then 1 else 0)) p


(* Problem 10 *)
val check_pat = 
let 
  fun get_var_names (Variable v) = [v] 
    | get_var_names (TupleP p) = List.foldl (fn (x, acc) => acc @ get_var_names(x)) [] p  
    | get_var_names (ConstructorP (_, p)) = get_var_names p  
    | get_var_names _ = []
  fun has_repeats xs = 
    case xs of
         [] => false
       | x::tl => (List.exists (fn y => x = y) tl) orelse has_repeats tl 
in
  not o has_repeats o get_var_names 
end


(* Problem 11 *)
fun match (v, p) =
  case (p, v) of 
    (Wildcard, _) => SOME []
  | (Variable s, _) => SOME [(s, v)]
  | (UnitP, Unit) => SOME []
  | (ConstP a, Const b) => if a = b 
                           then SOME [] 
                           else NONE
  | (TupleP ps, Tuple vs) => if List.length ps <> List.length vs 
                             then NONE
                             else all_answers match (ListPair.zip(vs, ps))
  | (ConstructorP (b, p), Constructor (a, v)) => if a <> b 
                                                 then NONE 
                                                 else match(v, p)  
  | _ => NONE


(* Problem 12 *)
fun first_match v ps = 
  SOME (first_answer (fn p => match(v, p)) ps) handle _ => NONE
