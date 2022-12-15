(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* if you use this function to compare two strings (returns true if the same string), then you avoid several of the functions in problem 1 having polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* PROBLEM 1*)
(* put your solutions for problem 1 here *)
(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you, to compare strings. Sample solution is around 8 lines. *)

fun all_except_option (s, slist) =
    let fun aux(s, slist, acc, res) = 
        case slist of 
        [] => if res then SOME acc else NONE
        | hslist::tslist => if same_string(s, hslist) then aux(s, tslist, acc, true) else aux(s, tslist, acc @ [hslist], res)   (* APPENDING A LIST TO BE IN THE CORRECT ORDER *)
    in 
        aux(s, slist, [], false)
    end


(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. 
Example: get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")

(* answer: ["Fredrick","Freddie","F"] *)

Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in substitutions. 
Example: get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

(* answer: ["Jeffrey","Geoff","Jeffrey"] *) *)  

fun get_substitutions1(string_lists, s) = 
    case string_lists of 
    [] => []
    | shead::stail => case all_except_option(s, shead) of 
                     NONE => get_substitutions1(stail, s)
                     | SOME result => result @ get_substitutions1(stail, s)        


(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function. *) 

fun get_substitutions2(string_lists, s) = 
    let fun aux(string_lists, s, acc) =
        case string_lists of 
        [] => acc
        | shead::stail => case all_except_option(s, shead) of 
                    NONE => aux(stail, s, acc)
                    | SOME result => aux(stail, s, acc @ result)
    in 
        aux(string_lists, s, [])
    end


(*  (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). 
The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names). 
Example: similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

(* answer: [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}] *)

Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is around 10 lines. *) 
           
fun similar_names(string_lists, full_name) = 
    let 
        val first_name = case full_name of  
                         {first=a,middle=b,last=c} => a

        fun change_name(new_names, full_name) = 
        case new_names of 
        [] => []
        | h_names::t_names => case full_name of 
                            {first=a,middle=b,last=c} => [{first= h_names, middle = b, last = c}] @ change_name(t_names, full_name) 
    in 
        let val names = get_substitutions2(string_lists, first_name)  
        in [full_name] @ change_name(names, full_name)
        end
    end


(* 2. This problem involves a solitaire card game invented just for this question. You will write a program that tracks the progress of a game; writing a game player is a challenge problem. You can do parts (a)–(e) before understanding the game if you wish.
A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player makes a move by either drawing, which means removing the first card in the card-list from the card-list and adding it to the held-cards, or discarding, 
which means choosing one of the held-cards to remove. The game ends either when the player chooses to make no more moves or when the sum of the values of the held-cards is greater than the goal. The objective is to end the game with a low score (0 is best). 
Scoring works as follows: Let sum be the sum of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal), else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual with integer division; use ML’s div operator). *)

(* you may assume that Num is always used with values 2, 3, ..., 10 though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds and hearts are red). Note: One case-expression is enough. *)

fun card_color card =
    case card of 
    (Spades, rank) => Black
    | (Clubs, rank) => Black
    | (_, rank) => Red   


(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)

fun card_value card = 
    case card of 
    (suit, Num int) => int      
    | (suit, Ace) => 11   
    | (suit, _) => 10


(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a list that has all the elements of cs except c. If c is in the list more than once, remove only the first one. 
If c is not in the list, raise the exception e. You can compare cards with =. *)

fun remove_card (cs, c, e) = 
    let fun aux(c, cs, acc, res) = 
        case cs of 
        [] => if res then acc else raise e
        | hcs::tcs => if c = hcs andalso res = false then aux(c, tcs, acc, true) else aux(c, tcs, hcs::acc, res)
    in 
        aux(c, cs, [], false)
    end


(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color. Hint: An elegant solution is very similar to one of the functions using nested pattern-matching in the lectures. *)

fun all_same_color (cs) = 
    let fun aux(cs, result) =
        case cs of
        [] => true
        | hcs::tcs => if card_color(hcs) = result then aux(tcs, result) else false
    in 
        case cs of 
        [] => true 
        | hcs::tcs => aux(tcs, card_color(hcs))
    end


(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined helper function that is tail recursive. 
(Take “calls use a constant amount of stack space” as a requirement for this problem.) *)

fun sum_cards cs = 
    let fun aux(cs, acc) =
        case cs of 
        [] => acc
        | hcs::tcs => aux(tcs, acc+card_value(hcs))  
    in 
        aux(cs, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above 

Let sum be the sum of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal), else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual with integer division; use ML’s div operator). *)

fun score(cs, goal) = 
    let 
        val sum_score = sum_cards cs
        val preliminary = if sum_score > goal then 3 * (sum_score - goal) else (goal - sum_score)
    in 
        if all_same_color(cs) then preliminary div 2 else preliminary
    end

(*  (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list (what the player “does” at each point), and an int (the goal) and returns the score at the end of the game after 
processing (some or all of) the moves in the move list in order. Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game. As described above:

• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger held-cards and a smaller card-list.

Sample solution for (g) is under 20 lines. *)

fun officiate (cs, moves, goal) =
    let fun aux(cs, moves, goal, hand) =
        case moves of
        [] => score(hand, goal) (* END GAME - COMPUTE SCORE*)
        | hm::tm => case hm of 
                    Discard c => aux(cs, tm, goal, remove_card(hand, c, IllegalMove))
                    | Draw => case cs of 
                                [] => score(hand, goal) (* END GAME - COMPUTE SCORE*)
                                | hcs::tcs => if sum_cards(hcs::hand) > goal then score(hcs::hand, goal) else aux(tcs, tm, goal, hcs::hand)     
        in 
            aux(cs, moves, goal, [])
        end


(* 3. Challenge Problems: *)

(* (a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that is less than or equal to the goal.) Hint: This is easier than you might think. *)

fun count_aces_and_value (cs) = 
    let fun aux(cs, number_aces, score_without_aces) = 
        case cs of 
        [] => (number_aces, score_without_aces)
        | hcs::tcs => case hcs of 
                    ( _ , Ace) => aux(tcs, number_aces+1, score_without_aces)   
                    | _ => aux(tcs, number_aces, score_without_aces+card_value(hcs)) 
        in aux(cs, 0, 0)
        end

fun score_challenge (cs, goal) =     
    let 
        fun calculate_score(value, goal) = if value > goal then 3 * (value - goal) else (goal - value)
        val result = count_aces_and_value(cs)
    in 
       let fun aux(result) =
            case result of 
            (a,b) => case a of 
                    0 => b 
                    | _ => if calculate_score(aux(a-1,b+1), goal) < calculate_score(aux(a-1,b+11), goal) then aux(a-1,b+1) else aux(a-1,b+11)
       in
            let val result = calculate_score(aux(result),goal)
            in if all_same_color(cs) then result div 2 else result    
            end        
       end 
    end


fun officiate_challenge (cs, moves, goal) =
    let fun aux(cs, moves, goal, hand) =
        case moves of
        [] => (case hand of 
              [] => goal
              | _ => score_challenge(hand, goal)) (* END GAME - COMPUTE SCORE*)

        | hm::tm => case hm of 
                    Discard c => aux(cs, tm, goal, remove_card(hand, c, IllegalMove))
                    | Draw => case cs of 
                                [] => (case hand of 
                                      [] => goal 
                                      | _ => score_challenge(hand, goal)) (* END GAME - COMPUTE SCORE*)

                                | hcs::tcs =>   let val result = count_aces_and_value(hcs::hand) 
                                                in 
                                                    case result of 
                                                    (a,b) => if (a * 1 + b) > goal then score_challenge(hand, goal) else aux(tcs, tm, goal, hcs::hand)   
                                                end  
        in 
            aux(cs, moves, goal, [])
        end

(*  (b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling officiate with the card-list, the goal, and the move-list has this behavior:

• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this must be done. Note careful_player will have to look ahead to the next card, which in many card
games is considered “cheating.” Also note that the previous requirement takes precedence: There must be no more moves after a score of 0 is reached even if there is another way to get back to 0. *)

(* REMINDER: The operator domain is what the function called expects, and the operand is what the function was called with. *)


(* DISCARD THE CARD THAT GIVES YOU THE BETTER SCORE (LESSER ONE) *)
fun discard_card_less_score (cs, goal) =
    let         
        fun aux(cs, card, lowest_score, hand) =         
            case cs of 
            [] => card
            |  hcs::tcs => if score(remove_card (hand, hcs, IllegalMove), goal) < lowest_score then aux(tcs, hcs, score(remove_card (hand, hcs, IllegalMove), goal), hand) else aux(tcs, card, lowest_score, hand)        
    in 
        case cs of          
        hcs::tcs => aux(tcs, hcs, score(remove_card (cs, hcs, IllegalMove), goal), cs)
    end

fun careful_player (cs, goal) =
    let     
        fun aux(cs, goal, hand, move_list, last_score) =
            case goal of
            0 => (0, move_list)
            | _ =>  case last_score of
                    0 => (last_score, move_list)  (* END GAME *)
                    | _ =>  case cs of 
                            [] =>   let
                                        val discard_card = discard_card_less_score(hand, goal)
                                        val new_hand = remove_card(hand, discard_card, IllegalMove)
                                        val new_score = score(new_hand, goal) 
                                    in  if new_score > last_score then (last_score, move_list) else aux(cs, goal, new_hand, Discard(discard_card)::move_list, new_score) 
                                    end    (* END GAME IF DISCARDING "THE BEST" CARD GIVES A WORSE OUTCOME*)                          
                        
                            | hcs::tcs =>   let
                                                val drawing_value = sum_cards(hcs::hand) 
                                                val drawing_score = score(hcs::hand, goal)                                                                              
                                            in  
                                                if drawing_value <= goal then aux(tcs, goal, hcs::hand, Draw::move_list, drawing_score)
                                                else 
                                                let 
                                                    fun discard_aux(fixed_hand, hand, move_list, last_score) = 
                                                        case hand of
                                                        [] => (last_score, move_list)
                                                        | head_hand::tail_hand => let 
                                                                                    val card1 = aux(cs, goal, remove_card(fixed_hand, head_hand, IllegalMove), Discard(head_hand)::move_list, score(remove_card(fixed_hand, head_hand, IllegalMove), goal))
                                                                                    val card2 = discard_aux(fixed_hand, tail_hand, move_list, last_score)
                                                                                    val card1_score = case card1 of (a,b) => a
                                                                                    val card2_score = case card2 of (a,b) => a    
                                                                                  in if card1_score < card2_score then card1 else card2
                                                                                  end  
                                                in  
                                                    discard_aux(hand, hand, move_list, last_score)                                                                
                                                end                                                                                                                                                                                                                  
                                            end               
    in 
        case cs of
        hcs::tcs => let 
                        val result = aux(tcs, goal, [hcs],[Draw], score([hcs], goal))  
                        fun reverse_list ls =
                            let fun aux(ls, acc) =
                                case ls of 
                                [] => acc
                                | hls::tls => aux(tls, hls::acc)

                            in aux(ls, [])
                            end                                                                       
                    in 
                        case goal of
                        0 => []
                        | _ =>  case result of 
                                (a,b) => reverse_list b
                    end
    end
