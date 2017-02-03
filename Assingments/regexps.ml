(* A2: regexps.ml *)

(** EXERCISE 0: Fill out the following README: *)

(* README

 Name:  Gabriel Lopez-Matthews
   ID:  P001235175
 Year:  Senior

 *)

open Batteries

(** * A2: Regular Expressions Re-Examined *)

(** ** Part I: Interpreting REs

    In this assignment, your job is to define an interpreter for a
    small language of regular expressions (type ['char regexp]
    below). The interpreter won't necessarily be efficient (in fact,
    it'll be asymptotically quite slow). For extra credit in Part II,
    you can speed things up through the use of "regular expression
    derivatives" (also known as "Brzozowski derivatives"). Although
    it's often more common to implement regular expression matchers
    via compilation first to NFAs then to DFAs (this is what lexers
    like ocamllex, which we'll use in the next assignment, do), RE
    derivatives present a fun and interesting alternative approach.

    Much of this assignment was developed using material from 

      "Regular-expression derivatives reexamined", 
      by Scott Owens, John Reppy, and Aaron Turon
      
   which is available here: 

      https://www.mpi-sws.org/~turon/re-deriv.pdf.
 *)

(** The following data type ['char regexp] defines a little language
    of regular expressions (recall the RE language we saw in class in
    Week 3 -- go back to the lecture notes to refresh your memory if
    necessary!). As reference, you may also take a look at Definition
    2.1 of the paper linked above.

    - For example, the constructor [Empty] denotes the RE 0 (which
      matches *no* strings). 
    - Likewise, the constructor [Epsilon] matches the empty string [""].
    - [Char c] matches the string containing just the character [c]
      (note that the type below is parameterized over the type of
      characters, ['char]).
    - [Concat(r1, r2)] matches strings of characters [s] that are
      splittable into two subtrings [s1] and [s2] such that [r1] matches
      [s1] and [r2] matches [s2].
    - [Or(r1, r2)] matches strings that are matched by either [r1] or [r2].
    - [And(r1, r2)] matches strings that are matched by both [r1] and [r2].
    - Finally, [Not r] matches strings that are *not* matched by [r].
 *)
       
type 'char regexp =
  | Empty
  | Epsilon
  | Char of 'char
  | Concat of 'char regexp * 'char regexp
  | Star of 'char regexp
  | Or of 'char regexp * 'char regexp
  | And of 'char regexp * 'char regexp
  | Not of 'char regexp

(* The following function, [pp_regexp], "pretty prints" regular
   expressions of type ['char regexp], by mapping each regular
   expression to a string. 

   Note that, since [regexp]s are parameterized by the type of
   characters they may contain ['char], the [pp_regexp]
   function is parameterized by a pretty-printing function for
   the ['char] type as well ([char_to_string : 'char -> string]).
 *)

let rec pp_regexp (char_to_string : 'char -> string) (r : 'char regexp) : string =
  match r with
  | Empty -> "Empty"    
  | Epsilon -> "'epsilon'"
  | Char c -> char_to_string c
  | Concat(r1, r2) -> pp_regexp char_to_string r1 ^ pp_regexp char_to_string r2
  | Star r' -> "(" ^ pp_regexp char_to_string r' ^ ")*"
  | Or(r1, r2) -> pp_regexp char_to_string r1 ^ "|" ^ pp_regexp char_to_string r2
  | And(r1, r2) -> pp_regexp char_to_string r1 ^ "&" ^ pp_regexp char_to_string r2
  | Not r' -> "not (" ^ pp_regexp char_to_string r' ^ ")"

(* EXERCISE 1: 

   Define a function, [string_to_re], that encodes a string of 
   OCaml characters (type [char]) as a [char regexp], an RE 
   specialized to characters of type [char]. For example, 

     string_to_re "a" = Concat (Char 'a', Epsilon)
     string_to_re ""  = Epsilon
     string_to_re "ab"= Concat (Char 'a', Concat (Char 'b', Epsilon)
     ...

   While implementing this function, you may find the Batteries
   [BatString] library useful (the documentation for which is 
   available here:

 http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatString.html).

   In particular, note the function [to_list : string -> char list]. 
   To call this function in the current program, you can prefix it by 
   its library name, as in: [BatString.to_list]. The same applies to 
   any batteries library function: [<BatLibraryName>.<function_name>]. 

 *)

 let rec string_to_re (s : string) : char regexp = 
  match BatString.to_list s with 
  | [] -> Epsilon
  | h::tail -> Concat(Char h, (string_to_re (BatString.of_list tail)))
;;

(** Recall from lecture that each RE denotes a *language*, i.e., a set
    of strings. Each string is a list of characters drawn from the 
    underlying alphabet, Sigma.

    We'll represent languages in OCaml as functions from strings to 
    bools. That is, a language is a function that, for each possible 
    string, returns true if that string is in the language and false 
    otherwise. 
 *)
	
type 'char language = 'char list -> bool

(* EXERCISE 2: 

   Define a function, [all_splits], that returns all possible
   "splittings" of an input character list [s]. What do I mean by
   "splitting" here? Just a partition of the characters in [s] into
   two strings [s1] and [s2] such that [s1 ^ s2 = s]. For example, 

     - ("", "abc") is a splitting of the string "abc" because 
       "" ^ "abc" = "abc".

     - Likewise, ("abc", "def") is a splitting of "abcdef". 

     - ("abcdef", "") also splits "abcdef".

  When defining [all_splits], you may find useful functions: 
     - [BatList.length]
     - [BatList.takedrop].
  Both of these functions are described in the Batteries documentation
  for the [BatList] module:

http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html.

 *)

let rec splits (s : 'char list) (n : int) : ('char list * 'char list) list = 
  match n with
  | 0 ->  BatList.takedrop n s :: []
  | n -> BatList.takedrop n s :: splits s (n-1) 
;;

let all_splits (s : 'char list) : ('char list * 'char list) list =
  splits s (BatList.length s)
;;

(* A test input: *)
(* all_splits [1; 2; 3; 4];; *)
(*expected: [([], [false;true]); ([false], [true]); ([false;true], [])]*)

(* EXERCISE 3: 

   Define a function, [interp], that maps regular expressions of type
   ['char regexp] to the languages (of type ['char language]) that
   they recognize. Your implementation of [interp] should be inspired
   quite closely by the mathematical definition of [interp] we
   discussed in class. As additional reference, you may find useful the 
   mathematical definition of the function L[[ . ]] in Section 2.1 
   of "Regular-expression derivatives reexamined". 

   HINT: Be particularly careful in the [Star] case. The naive
   interpretation of [Star] (e.g., following "Regular-expression
   derivatives reexamined" Section 2.1 verbatim) will probably cause
   your interpretation function to infinite loop. Consider an
   alternative implementation (inspired perhaps by the second
   interpretation I showed in class :-)).

 *)

let rec interp (r : 'char regexp) : 'char language =
  match r with
  | Empty -> fun my_string -> false
  | Epsilon -> fun my_string -> if my_string = [] then true else false
  | Char x -> fun my_string -> if my_string = [x] then true else false
  | Concat (x, y) -> fun my_string -> let (hd, tail) = BatList.takedrop 1 my_string in 
    if (interp x hd && interp y tail) then true else false
(*
  | Star x -> fun s ->  
*)
  | Or (x, y) -> fun my_string -> if interp x my_string then true else if interp y my_string then true else false
  | And (x, y) -> fun my_string -> if interp x my_string && interp y my_string then true else false
  | Not x -> fun my_string -> if interp x my_string then false else true
  | _ -> fun _ -> false
;;


let string_interp (r : char regexp) : string -> bool =
  fun s -> interp r (BatString.to_list s)
;;
(* EXERCISE 4: 

   The test harness below provides a generic mechanism for testing an
   interpretation function, like your [interp] above, against a number
   of (regular expression, input string) pairs.  A few test cases are
   given for you. Add at least 10 more. And don't be afraid to get
   creative! We'll award BONUS points if you come up with a test case
   that breaks our reference implementation.

 *)

type expectedResult = Pass | Fail
				     
let test1 (interp : char regexp -> string -> bool) (e : expectedResult) (r : char regexp) (s : string) : unit =
  let char_to_string c = BatString.of_list [c] in
  let show_regexp r = pp_regexp char_to_string r in
  let print_result msg = BatPrintf.printf "  %s: %s \"%s\"\n" msg (show_regexp r) s in
  let result = interp r s in
  match (result, e) with
  | (true, Pass) -> print_result "pass"
  | (false, Pass) -> print_result "*** FAILED"
  | (true, Fail) -> print_result "*** PASSED"
  | (false, Fail) -> print_result "fail"
		     
let test (testfun : expectedResult -> char regexp -> string -> 'a) : unit =
  let _ = BatList.map (fun (e,r,s) -> testfun e r s)
  [ 
    (Fail, Empty, "abcde"); (* we expect the Empty regexp to fail on string "abcde" *)
  	(Fail, Epsilon, "abcde"); 
  	(Pass, Epsilon, ""); (* we expect Epsilon to succeed on "" *)
  	(Pass, string_to_re "a", "a");
  	(Pass, string_to_re "abc", "abc");
    (Pass, Char 'a', "a");  (* 1 *)
    (Fail, Epsilon, "");    (* 2 *)
    (Fail, Char 'a', "a");  (* 3 *)
    (Pass, Concat(string_to_re "ab", string_to_re "a"), "aba"); (* 4 *)
    (Pass, Concat(string_to_re "ab", string_to_re "a"), "ab");  (* 5 *)
    (Pass, Not(Char 'a'), "a"); (* 6 *)
    (Fail, Not(Char 'a'), "a"); (* 7 *)
    (Pass, And(Char 'a', Char 'a'), "a"); (* 8 *)
    (Fail, And(Char 'a', Char 'a'), "a"); (* 9 *)
    (Pass, Or(Char 'a', Char 'b'), "a");  (* 10 *)
    (Pass, Or(Char 'a', Char 'b'), "b");  (* 11 *)
    (Pass, Or(Char 'a', Char 'b'), "c");  (* 12 *)
    (Fail, Or(Char 'a', Char 'b'), "a");  (* 13 *)
  ]
  in ();;
  
test (test1 string_interp);;  

(** ** Part II: Regular Expression Derivatives 

    In Part I, you defined an interpreter [interp] that mapped REs to
    the languages they represented. The interpreter was kind of dumb,
    in the sense that it followed the mathematical definition we gave
    in class quite closely, even when that caused a performance hit
    (hint: when interpreting [Concat(r1, r2)], your [interp] probably
    explored all possible "splittings" of the input string).

    In Part II, we'll use RE derivatives to implement a more efficient
    regular expression matcher. This part of the assignment follows 
    Sections 3.1 and 3.2 of "Regular-expression derivatives reexamined". 
*)

(* EXERCISE 6: 

   (Re-)read Section 3.1 of "Regular-expression derivatives reexamined". 

   Following the definition of the "nu" (greek letter nu) function 
   given in Section 3.1, define a function, called [nullable], that 
   returns [true] if a regexp [r] accepts the empty string and 
   [false] otherwise. 

 *)
  
let rec nullable (r : 'char regexp) : bool =
  (* FILL IN DEFINITION HERE *) false
;;

(* Some test inputs: *)  
nullable Epsilon;; (*expected: true*)
nullable Empty;; (*expected: false*)
nullable (Concat(Epsilon, Epsilon));; (*expected: true*)

(* EXERCISE 7:

   Following the definition of the "delta" function given in 
   Section 3.1, define a function [deriv] that returns the 
   derivative of a regular expression [r] with respect to any 
   given character [a : 'char]. For example,  

     deriv 'a' (Star (Char 'a')) = Concat (Epsilon, Star (Char 'a'))
                  
 *)
		  
let rec deriv (a : 'char) (r : 'char regexp) : 'char regexp =
  (* FILL IN DEFINITION HERE *) Empty
;;				  

(* A test input: *)
deriv 'a' (Star (Char 'a'));; (*expected: Concat (Epsilon, Star (Char 'a'))*)

(* EXERCISE 8:

   Re-read Section 3.2 of "Regular-expression derivatives reexamined". 

   Following the definition of the "matches" relation "r ~ s" in
   Section 3.2, define a function [matches] that takes as arguments 
     - a regular expression [r : 'char regexp]
     - a string [s : 'char list] 
   and returns [true] if [r] matches [s] and [false] otherwise. 

   For example, 
   
     matches (Char a) [a] = true
     matches (Star (Char a)) [] = true
     matches (Or (Char a, Char b)) [] = false
     matches (Or (Char a, Char b)) [a; b] = false
     matches (Or (Char a, Char b)) [b] = true

 *)
			    
let rec matches (r : 'char regexp) (s : 'char list) : bool =
  (* FILL IN DEFINITION HERE *) false
;;

(* Test inputs: *)
matches (Char 'a') ['a'];; (*expected: true*)
matches (Star (Char 'a')) [];; (*expected: true*)
matches (Or (Char 'a', Char 'b')) [];; (*expected: false*)
matches (Or (Char 'a', Char 'b')) ['a'; 'b'];; (*expected: false*)
matches (Or (Char 'a', Char 'b')) ['b'];; (*expected: true*)
				  

(* EXERCISE 9:

   Using [BatString.to_list], define a wrapper around [matches]
   that checks whether an RE [r : char regexp] (that is, specialized 
   to OCaml characters) matches an OCaml string [s]. 

 *)
		       
let string_matches (r : char regexp) : string -> bool =
  (* FILL IN DEFINITION HERE *)
  fun s -> false
;;		   

(*Some test inputs:*)
string_matches (Char 'a') "a";; (*expected output: true*)
string_matches (Star (Char 'a')) "";; (*expected output: true*)
string_matches (Star (Char 'a')) "aaa";; (*expected output: true*)
string_matches (Star (Char 'a')) "aba";; (*expected output: false*)          
  
(* test (test1 string_matches);; *)