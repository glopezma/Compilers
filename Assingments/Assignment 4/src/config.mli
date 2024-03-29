(** The [Config] module defines target-specification information
    relevant to code generation. *)

(** The [target_triple] specifies the LLVM compilation target, and is
    included in every LLVM assembly file generated by the compiler. *)
val target_triple : string ref
