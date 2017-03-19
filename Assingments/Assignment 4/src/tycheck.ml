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
  | EId x -> 
     (match Symtab.get x gamma with
      | None -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf "unbound identifier '%a'@ at position %a" pp_id x pp_pos e))
      | Some t -> { e with exp_of = EId x; ety_of = t })
  (* | ESeq    ->  raise_ty_err "Unimplemented"
  | ECall   ->  raise_ty_err "Unimplemented"
  | ERef    ->  raise_ty_err "Unimplemented" *)
  | EUnop(u, e1)      -> tycheck_unop e gamma u e1
  | EBinop(b, e1, e2) -> tycheck_binop e gamma b e1 e2
  (* | EIf     ->  raise_ty_err "Unimplemented"
  | ELet    ->  raise_ty_err "Unimplemented"
  | EScope  ->  raise_ty_err "Unimplemented" *)
  | EUnit   ->  { e with exp_of = EUnit;    ety_of = TyUnit }
  | ETrue   ->  { e with exp_of = ETrue;    ety_of = TyBool }
  | EFalse  ->  { e with exp_of = EFalse;   ety_of = TyBool }
  (* | EWhile  ->  raise_ty_err "Unimplemented" *)
  | _ -> raise_ty_err "Unimplemented"
  

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
      else raise_ty_err "Wasn\'t of type Float or Int"
		  

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
  | UDeref  ->  raise_ty_err "Unimplemented" 

		  
and tycheck_binop (e : 'a exp) (gamma : ty_env)
                  (b : binop) (e1 : 'a exp) (e2 : 'a exp) : ty exp =
  raise_ty_err "Unimplemented"        

(** [tycheck_fundef f]:
    Checks that function [f] is well-typed. 
    Returns a type-annotated version of [f]. *)
	 
let tycheck_fundef (f : (ty, 'a exp) fundef) : (ty, ty exp) fundef =
  raise_ty_err "Unimplemented"          

(** [tycheck_prog p]:
    Checks that program [p] is well-typed. 
    Returns a type-annotated version of [p]. *)
		  
let tycheck_prog (p : (ty, 'a exp) prog) : (ty, ty exp) prog =
  raise_ty_err "Unimplemented"            
