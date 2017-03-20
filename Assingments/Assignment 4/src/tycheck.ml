open Batteries
open BatFormat

open Lexing

open Lexer
open AST
open Exp

(* 
  Name: Gabriel Lopez-Matthews 
  OUID: P001235175

  Name: Rhen Daffin
  OUID: P100189836
*)


(** Declare a new exception type, Ty_error, which takes as 
    its first argument a message describing the type error *)
       
exception Ty_error of string 

(** Given an error string, raise the associated type error *)
			
let raise_ty_err (err : string) = raise (Ty_error err)

(** The type of type environments \Gamma, mapping identifiers to the
    types they've been assigned *)
					
type ty_env = ty Symtab.t

(** The type of the global environment \Delta, mapping function names 
    (identifiers) to the types of their arguments and to their 
    result type *)
		 
type globs_env = (ty list * ty) Symtab.t

(** delta is a file-scope reference to the global type environment *)
				
let delta : globs_env ref = ref (Symtab.create ())

(** Return the argument types and return type associated with 
    function id [f] in [delta], or raise a type error. *)
				
let ety_of_funid (f : id) : (ty list * ty) =
  match Symtab.get f !delta with
  | None ->
     raise_ty_err
       (pp_to_string
	  (fun ppf ->
	   fprintf ppf "unbound function identifier '%a'" pp_id f))
  | Some tys -> tys

(** Is id [x] bound in environment [gamma]? *)
				
let is_bound (gamma : ty_env) (x : id) : bool =
  BatOption.is_some (Symtab.get x gamma)

(** The tycheck function checks whether [e] is well-typed in 
    environment [gamma]. If so, the function returns the new 
    expression equal to [e] but annotated with its type (in 
    the [ety_of] field -- see type ['a exp] in [exp.mli] for 
    additional information). *)

let rec tycheck (gamma : ty_env) (e : 'a exp) : ty exp =
  match e.exp_of with
  | EInt    i   ->  { e with exp_of = EInt i;   ety_of = TyInt }
  | EFloat  f   ->  { e with exp_of = EFloat f; ety_of = TyFloat }
  | EId x       ->  (match Symtab.get x gamma with
                      | None -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf "unbound identifier '%a'@ at position %a" pp_id x pp_pos e)) (*GO THROUGH LATER TO FILL THIS IN INSTEAD OF STUPID WARNING ^^ MUCH BETTER WARNINGS*)
                      | Some t -> { e with exp_of = EId x; ety_of = t })
  (* | ESeq    ->  raise_ty_err "Unimplemented" *)

  (*Fix later because this is too close to Nathan's and Bailey's answer *)
  | ECall (x, y)  ->  let (argList, argType) = ety_of_funid x in
                      let y' = BatList.map2 (fun i j -> assert_ty gamma i j) y argList in
                      { e with
                        exp_of = ECall (x, y');
                        ety_of = argType
                      }
                        
  | ERef x  ->  let x' = tycheck gamma x in {e with exp_of = ERef x';   ety_of = TyRef x'.ety_of}
  | EUnop (u, e1)      -> tycheck_unop e gamma u e1
  | EBinop (b, e1, e2) -> tycheck_binop e gamma b e1 e2
  | EIf (e1, e2, e3)  ->  let e1' = assert_ty gamma e1 TyBool in
                          let e2' = tycheck gamma e2 in
                          let e3' = assert_ty gamma e3 e2'.ety_of in
                          { e with 
                            exp_of = EIf(e1', e2', e3');
                            ety_of = e3'.ety_of
                          }

  | ELet (i, e1, e2)  ->  let e1' = tycheck gamma e1 in
                          let e2' = tycheck (Symtab.set i e1'.ety_of gamma) e2 in (*Should have some sort of check for bounding errors*)
                          {e with 
                            exp_of = ELet(i, e1', e2');
                            ety_of = e2'.ety_of
                          }
  | EScope e1  ->   let e1' = tycheck gamma e1 in
                    { e with
                      exp_of = EScope e1';
                      ety_of = e1'.ety_of
                    }
  | EUnit   ->  { e with exp_of = EUnit;    ety_of = TyUnit }
  | ETrue   ->  { e with exp_of = ETrue;    ety_of = TyBool }
  | EFalse  ->  { e with exp_of = EFalse;   ety_of = TyBool }
  | EWhile(e1, e2)  ->  let e1' = assert_ty gamma e1 TyBool in 
                        let e2' = assert_ty gamma e2 TyUnit in 
                        { e with 
                          exp_of = EWhile(e1', e2');
                          ety_of = TyUnit
                        }
  

(** [assert_ty gamma e t]: Raise a type error if [e] does not 
    have type [t] in [gamma]. Returns a type-annotated version 
    of [e] (just as in [tycheck]) *)
	  
and assert_ty (gamma : ty_env) (e : 'a exp) (t : ty) : ty exp =
  let e' = tycheck gamma e in 
  if ty_eq e'.ety_of t then e'
  else raise_ty_err "Expected type Bool"

(** [assert_arith gamma e]: Raise a type error if [e] does not have an
    arithmetic type (see [exp.ml] and [exp.mli] for the definition of
    "arithmetic type". Returns a type-annotated version of [e]
    (just as in [tycheck]) *)
	    
(*  NOTES TO HELP STUPID GABE
    ety_of == the TyType 
    exp_of == Etype + expression 
              EInt  + "3"

    Given False
    ety_of False == TyBool
    exp_of False == EFalse + False (same thing as ty exp)
*)
and assert_arith (gamma : ty_env) (e : 'a exp) : ty exp =
(** [tycheck_unop e gamma u e2]: 
    Assumes [e = EUnop(u, e2)]. 
    Checks that [EUnop(u, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)
    let e' = tycheck gamma e in 
      if ty_eq e'.ety_of TyInt then e' 
      else if ty_eq e'.ety_of TyFloat then e'
      else raise_ty_err "Expected Float or Int"
		  

and tycheck_unop (e : 'a exp) (gamma : ty_env) (u : unop) (e2 : 'a exp) : ty exp = 
(** [tycheck_binop e gamma b e1 e2]: 
    Assumes [e = EBinop(b, e1, e2)]. 
    Checks that [EBinop(b, e1, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)
  match u with  (*  e' = type of TyBool *)  
  | UNot    ->  let e' = assert_ty gamma e2 TyBool in
                { e with 
                  exp_of = EUnop(u, e');
                  ety_of = TyBool
                }
                (*  e' = type of either TyFloat or TyInt*)     
  | UMinus  ->  let e' = assert_arith gamma e2 in 
                  { e with 
                    exp_of = EUnop(u, e');
                    ety_of = e'.ety_of
                  }
  | UDeref  ->  let e' = tycheck gamma e2 in (*BEFORE WE TURN IN, FIGURE OUT HOW OTHER PEOPLE SOLVED THIS, WE NO ABLE TO SOLVE CUZ DUMB*)
                  (match e'.ety_of with 
                    | TyRef(x) -> 
                                { e with
                                  exp_of = EUnop(u, e');
                                  ety_of = x
                                }
                    | x -> raise_ty_err "Expected reference type!"
                  )

		  
and tycheck_binop (e : 'a exp) (gamma : ty_env) (b : binop) (e1 : 'a exp) (e2 : 'a exp) : ty exp =
    match b with
    | BPlus ->  let e1' = assert_arith gamma e1 in
                let e2' = assert_arith gamma e2 in
                if ty_eq e1'.ety_of e2'.ety_of then 
                { e with
                  exp_of = EBinop(b, e1', e2');
                  ety_of = e1'.ety_of
                }
                else raise_ty_err "Expected BPlus"
    (* A lot of copy and pasting for these! *)
    | BMinus  ->  let e1' = assert_arith gamma e1 in
                  let e2' = assert_arith gamma e2 in
                  if ty_eq e1'.ety_of e2'.ety_of then 
                  { e with
                    exp_of = EBinop(b, e1', e2');
                    ety_of = e1'.ety_of
                  }
                  else raise_ty_err "Expected BMinus"

    | BTimes  ->  let e1' = assert_arith gamma e1 in
                  let e2' = assert_arith gamma e2 in
                  if ty_eq e1'.ety_of e2'.ety_of then 
                  { e with
                    exp_of = EBinop(b, e1', e2');
                    ety_of = e1'.ety_of
                  }
                 else raise_ty_err "Expected BTimes"

    | BDiv  ->  let e1' = assert_arith gamma e1 in
                let e2' = assert_arith gamma e2 in
                if ty_eq e1'.ety_of e2'.ety_of then 
                { e with
                  exp_of = EBinop(b, e1', e2');
                  ety_of = e1'.ety_of
                }
                else raise_ty_err "Expected BDiv"


    | BAnd  ->  let e1' = assert_ty gamma e1 TyBool in
                let e2' = assert_ty gamma e2 TyBool in
                if ty_eq e1'.ety_of e2'.ety_of then 
                { e with
                  exp_of = EBinop(b, e1', e2');
                  ety_of = TyBool
                }
                else raise_ty_err "Expected BAnd"

    | BOr   ->  let e1' = assert_ty gamma e1 TyBool in
                let e2' = assert_ty gamma e2 TyBool in
                if ty_eq e1'.ety_of e2'.ety_of then 
                { e with
                  exp_of = EBinop(b, e1', e2');
                  ety_of = TyBool
                }
                else raise_ty_err "Expected BOr"

    | BLt   ->  let e1' = assert_arith gamma e1 in
                let e2' = assert_arith gamma e2 in
                if ty_eq e1'.ety_of e2'.ety_of then 
                { e with
                  exp_of = EBinop(b, e1', e2');
                  ety_of = TyBool
                }
               else raise_ty_err "Expected BLt"

    | BIntEq  ->  let e1' = assert_ty gamma e1 TyInt in
                  let e2' = assert_ty gamma e2 TyInt in
                  if ty_eq e1'.ety_of e2'.ety_of then 
                  { e with
                    exp_of = EBinop(b, e1', e2');
                    ety_of = TyBool
                  }
                  else raise_ty_err "Expected BOr"

    | BUpdate ->  let e1' = tycheck gamma e2 in 
                  (match e1'.ety_of with 
                    | TyRef(x) -> let e2' = assert_ty gamma e2 x in
                                { e with
                                  exp_of = EBinop(b, e1', e2');
                                  ety_of = TyUnit
                                }
                    | y -> raise_ty_err "Expected reference type!"
                  )      


(** [tycheck_fundef f]:
    Checks that function [f] is well-typed. 
    Returns a type-annotated version of [f]. *)
	 
let tycheck_fundef (f : (ty, 'a exp) fundef) : (ty, ty exp) fundef =
  raise_ty_err "Unimplemented fundef"         


(** [tycheck_prog p]:
    Checks that program [p] is well-typed. 
    Returns a type-annotated version of [p]. *)
(* Given by Alexander Bagnall in the Piazza group chat *)
let tycheck_prog (p : (ty, 'a exp) prog) : (ty, ty exp) prog =
  let typ = tycheck (Symtab.create ()) p.result in
  { fundefs = [];
    result = typ }
(* Using algorithm found on stack overflow *)
(* let size l = mylist.fold_left (fun acc _ -> acc + 1) 0 l;; *)
