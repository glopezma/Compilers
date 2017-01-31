(*
	Pythagorean Theorem & Area: 
		1. write down the function and argument names
		2. write down arguemnt and result types
		3. write down some examples (in a coment) 
		4. deconstruct input data structures
			* the argument types suggests hwo to do it
		5. build new output values
			* the result types suggests hwo you do it
		6. clean up by identifying repeated patterns
			* define and reuse helper functions
			* your code should be elegant and easy to read
*)

type point = float * float;; (* type abreviation *)
let distance (p1:point) (p2:point) : float = 
	(*  Examples:
		distance (0.0, 0.0) (0.0, 1.0) == 1.0
		distance (0.0, 0.0) (1.0, 1.0) == sqrt (1.0 + 1.0)

		From picture:
		distance (x1, y1) (x2, y2) == sqrt (a^2 + b^2)
	*)
	let square x = x *. x in
	let (x1, y1) = p1 in
	let (x2, y2) = p2 in
	(* sqrt((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)) *)
	sqrt(square (x2 -. x1) +. 
		 square (y2 -. y1))
;;

let pt1 = (2.0, 3.0);;
let pt2 = (0.0, 1.0);;
let dist12 = distance pt1 pt2;; (*testing*)

type point = float * float;;

let abs (f: float) : float = 
	if f<0.0 then -1.0 *. f
	else f

let area (p1:point) (p2:point) : float = 
	(* Examples: 
		area (0.0, 0.0) (1.0, 1.0) = 1.0
		area (0.0, 0.0) (1.0, 0.0) = 0.0
	*)
	let (x1, y1) = p1 in
	let (x2, y2) = p2 in
	let height = abs (y2-.y1) in
	let length = abs (x2 -. x1) in
	height *. length

;;

(* type ledit ocaml to get extra stuff to help with command stuff! *)

(* 
	glopezma@GABESSP3:/mnt/c/Users/Gabriel/Dropbox/Documents/Fifth Year/Semester 2/Compilers$ ocaml
        OCaml version 4.02.3                                                                                             intro.cmi  intro.ml  sumTo.ml
                                                                                                                     ml  intro.cmx  intro.o   Thumbs.db
		# #use "pythagoriumTheorem.ml"
		  ;;
		Cannot find file pagoriumTheorem.ml.
		# #use "pythagoriumTheorem.ml";;
		type point = float * float
		val distance : point -> point -> float = <fun>
		val pt1 : float * float = (2., 3.)
		val pt2 : float * float = (0., 1.)
		val dist12 : float = 2.82842712474619029
		type point = float * float
		val area : point -> point -> float = <fun>
		# #use "pythagoriumTheorem.ml";;
		type point = float * float
		val distance : point -> point -> float = <fun>
		val pt1 : float * float = (2., 3.)
		val pt2 : float * float = (0., 1.)
		val dist12 : float = 2.82842712474619029
		type point = float * float
		val abs : float -> float = <fun>
		val area : point -> point -> float = <fun>
		# area (0.0, 0.0) (1.0, 1.0)
		  ;;
		- : float = 1.
		# #use "pythagoriumTheorem.ml";;
		File "pythagoriumTheorem.ml", line 50, characters 1-4:
		Error: Syntax error
		# #use "pythagoriumTheorem.ml"
		  ;;
		type point = float * float
		val distance : point -> point -> float = <fun>
		val pt1 : float * float = (2., 3.)
		val pt2 : float * float = (0., 1.)
		val dist12 : float = 2.82842712474619029
		type point = float * float
		val abs : float -> float = <fun>
		val area : point -> point -> float = <fun>
		#

 *)