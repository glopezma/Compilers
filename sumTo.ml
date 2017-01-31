(*
	This is a comment, because we now know how to do that! :D
*)

let rec sumTo (n:int) : int = 
	match n with
	0 -> 0
	| m -> m + sumTo (n-1)
;;